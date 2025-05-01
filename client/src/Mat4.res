type mat4
type vec3
@module("mat4") external create : unit => mat4 = "create"
@module("mat4") external perspective : (mat4, float, float, float, float) => mat4 = "perpective"
@module("mat4") external translate : (mat4, mat4, vec3) => mat4 = "translate"