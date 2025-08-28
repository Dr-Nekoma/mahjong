(defmodule session
  (doc "REST hello world handler.")
  (export
    (init 2)
    (player 2)
    (waiting-player 1)
    (full-state->player-state 2)
    (available-actions 1)
    (room 1))
  (export (to_html 2))
  (module-alias (collections coll)))

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))

(defun public-information ()
  '(discard-pile open-hand))

(defun extract-if-self (player-number)
  (lambda (player index)
    (if (== index player-number)
      player
      (maps:with (public-information) player))))

(defun full-state->player-state (state player-number)
  (let ((players (coll:tmap
                   (extract-if-self player-number)
                   (mref state 'players))))
    (mset (maps:with '(current-player) state)
      'players players
      'you player-number)))

(defun available-actions (player-state)
  (lists:flatmap
    (lambda (f)
      (funcall f player-state))
    (list
      ;; TODO: add more checks
      ;; TODO: abstract common pattern
      (lambda (player-state)
        (if (yaku:call-riichi?
              (coll:get-in player-state
              (list 'players (mref player-state 'you) 'hand)))
          (list 'call-riichi)
          (list))))))

;; state: hand[2-man, 3-man, 1-pin, ...], last-command['discard], last-player[1], whoami[2]
;; returns: ['can-call-chi]
(defun player (state number)
  (receive
    (`#(new-state ,state)
     (let ((visible-state (full-state->player-state state number)))
       ;; TODO: push this to the frontend
       (available-actions visible-state))
     (player state number))
    (unknown
     (io:format "Unknown message: ~p\n" (list unknown))
     (player state number))))

(defun waiting-player (number)
  (receive
    (`#(ready? ,another-player-number)
     (io:format "Player ~p is ready!\n" (list another-player-number))
     ;; TODO: We need to send a message to FE to inform that
     )
    (`#(all-ready! ,initial-state)
     (io:format "Everything set! Ready, go!\n" (list))
     (player initial-state number))))

;; state :: {players :: { number :: int; pid :: bigint; ready? :: bool } list;
;;           decider-pid :: bigint}
;; TODO: This does not take into account people disconnecting or closing the browser.
;; We need to save more state (likely via mnesia) to recover from those situations.
(defun room (state)
  (receive
    (`#(connect ,http-id)
     (let* ((players (map-get state 'players))
	    (players-count (erlang:length players))
	    (player-number (+ players-count 1)))
       (if (< players-count 4)
	 (progn
	   (! http-id `#(connected ,player-number))
	   (room (coll:update-in state '(players)
			 (cons (tuple player-number
				      (spawn 'session 'waiting-player `(,player-number))
				      'false)  players))))
	 (progn
	   (! http-id 'room-is-full)
	   (room state)))))
    (`#(ready ,http-id ,player-id)
     (progn
       (! http-id `#(ready ,player-id))
       ;; updates the ready status of each player,
       ;; which is the third value in the tuple
       (room (coll:update-in state `(players ,player-id 3) 'true))))
    (`#(start ,http-id ,player-id)
     (let* ((owner (coll::get-in '(players 4) state))
	    (players (map-get state 'players))
       	    (ready-players (lists:filter (lambda (player) (== 'true (coll:get-in '(3) player))) players))
	    (ready-players-count (erlang:length ready-players)))
       (if (and (== ready-players-count 4) (== owner player-id))
	 (let* ((initial-game-state (game:initial-game))
		(decider-id (spawn 'game 'decider initial-game-state)))
	   (lists:foreach (lambda (player)
			    (let ((waiting-player-process (lists:nth 2 player)))
			      (! waiting-player-process `#(all-ready! initial-game-state))))
			  (map-get state 'players))
	   (! http-id 'about-to-start)	   
	   (room (map-set state 'decider-id decider-id)))
	 (! http-id `#(error "Either not all players are ready or someone started without being owner of the room")))))
    (`#(play ,http-id ,action ,player-id)
     (progn
        (! (map-get state 'decider-id) (tuple action (map 'player player-id 'pid (self))))
	(receive
	  ((tuple 'success new-state)
	   (lists:foreach (lambda (player) (! (lists:nth 2 player) `#(new-state ,new-state))) (map-get state 'players))
	   (io:format "Success: ~p\nWall length: ~p\n" (list new-state (length (map-get new-state 'wall))))
	   (! http-id 'played)	   
	   (room state))
	  ((tuple 'error msg)
	   (io:format "Error: ~p\n" (list msg))
	   (! http-id 'garbagio)
	   (room state))
	  (anything (io:format "Catchall: ~p\n" (list anything))))))))

(defun body ()
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Mahjong Room</title><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"><style>:root{--bg:#fff;--fg:#1f2937;--muted:#6b7280;--accent:#2563eb;--good:#059669;--warn:#d97706;--bad:#dc2626;--row:#f8fafc;--ring:#e5e7eb}@media(prefers-color-scheme:dark){:root{--bg:#0b1020;--fg:#e5e7eb;--muted:#9ca3af;--accent:#60a5fa;--good:#34d399;--warn:#fbbf24;--bad:#f87171;--row:#111827;--ring:#1f2937}}body{margin:0;font-family:ui-sans-serif,system-ui,-apple-system,\\\"Segoe UI\\\",Roboto,\\\"Helvetica Neue\\\",Arial,\\\"Noto Sans\\\",\\\"Liberation Sans\\\",\\\"Apple Color Emoji\\\",\\\"Segoe UI Emoji\\\";background:var(--bg);color:var(--fg)}.wrap{max-width:760px;margin:40px auto;padding:0 16px}h1{font-size:1.25rem;margin:0 0 12px}.sub{color:var(--muted);font-size:.9rem;margin-bottom:16px}table{width:100%;border-collapse:collapse;background:var(--bg);border:1px solid var(--ring);border-radius:12px;overflow:hidden}caption{caption-side:top;padding:12px 14px;font-weight:600;text-align:left}thead th{text-align:left;font-size:.85rem;letter-spacing:.02em;color:var(--muted);padding:10px 12px;background:var(--row);border-bottom:1px solid var(--ring)}tbody td{padding:12px;border-bottom:1px solid var(--ring);vertical-align:middle}tbody tr:last-child td{border-bottom:0}.seat{white-space:nowrap;font-variant-numeric:tabular-nums}.seat .wind{font-size:1.05rem;margin-right:.4rem}.player{display:flex;align-items:center;gap:10px}.player .tag{font-size:.7rem;padding:2px 8px;border-radius:999px;border:1px solid var(--ring);color:var(--muted)}.dealer{color:var(--accent);font-weight:600}.score{font-variant-numeric:tabular-nums}.pos{color:var(--good)}.neg{color:var(--bad)}.riichi{font-variant-numeric:tabular-nums}.pill{display:inline-flex;align-items:center;gap:6px;padding:4px 10px;border-radius:999px;border:1px solid var(--ring);font-size:.8rem;white-space:nowrap}.yes{color:var(--good)}.no{color:var(--muted)}.status{display:inline-flex;align-items:center;gap:6px}.dot{width:8px;height:8px;border-radius:999px;background:var(--good);display:inline-block}.dot.off{background:var(--bad)}.actions{color:var(--muted);font-size:.9rem}@media(max-width:560px){.hide-sm{display:none}caption{padding-bottom:0}}</style></head><body><div class=\"wrap\"><h1>Room A • East 2 (1/4)</h1><div class=\"sub\">Round: E2 • Honba: 1 • Riichi Pool: 2</div><table role=\"table\" aria-label=\"Mahjong room player table\"><caption>Players</caption><thead><tr><th scope=\"col\">Seat</th><th scope=\"col\">Player</th><th scope=\"col\" class=\"hide-sm\">Dealer</th><th scope=\"col\">Score</th><th scope=\"col\">Riichi</th><th scope=\"col\">Tenpai</th><th scope=\"col\" class=\"hide-sm\">Connection</th><th scope=\"col\" class=\"hide-sm\">Last Action</th></tr></thead><tbody><tr><td class=\"seat\"><span class=\"wind\">東</span> East</td><td class=\"player\"><span class=\"name\">Marcos</span><span class=\"tag\">Host</span></td><td class=\"dealer\">Dealer</td><td class=\"score pos\">27,900</td><td class=\"riichi\">1 stick</td><td><span class=\"pill yes\">Yes</span></td><td class=\"status hide-sm\"><span class=\"dot\"></span> Online</td><td class=\"actions hide-sm\">Discarded 3m</td></tr><tr><td class=\"seat\"><span class=\"wind\">南</span> South</td><td class=\"player\"><span class=\"name\">Anna</span></td><td>—</td><td class=\"score pos\">31,200</td><td class=\"riichi\">0</td><td><span class=\"pill no\">No</span></td><td class=\"status hide-sm\"><span class=\"dot\"></span> Online</td><td class=\"actions hide-sm\">Called Pon (🀙)</td></tr><tr><td class=\"seat\"><span class=\"wind\">西</span> West</td><td class=\"player\"><span class=\"name\">Ken</span></td><td>—</td><td class=\"score neg\">22,000</td><td class=\"riichi\">1 stick</td><td><span class=\"pill no\">No</span></td><td class=\"status hide-sm\"><span class=\"dot off\"></span> Offline</td><td class=\"actions hide-sm\">Draw</td></tr><tr><td class=\"seat\"><span class=\"wind\">北</span> North</td><td class=\"player\"><span class=\"name\">Lu</span></td><td>—</td><td class=\"score pos\">38,900</td><td class=\"riichi\">0</td><td><span class=\"pill yes\">Yes</span></td><td class=\"status hide-sm\"><span class=\"dot\"></span> Online</td><td class=\"actions hide-sm\">Riichi</td></tr></tbody></table></div></body></html>")

(defun to_html (req _state)
  "Return a text hello."
  (io:format "Request: ~p\n" (list req))
  (let ((room-pid (spawn 'session 'room (list (map))))
	(body-str (body))
    `#(,body-str ,req ,state))))
