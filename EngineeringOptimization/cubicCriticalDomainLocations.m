cubicCriticalDomainLocations[(y1_)?NumericQ, (x1_)?NumericQ, (y2_)?NumericQ, 
     (x2_)?NumericQ, (y3_)?NumericQ, (x3_)?NumericQ, (y4_)?NumericQ, 
     (x4_)?NumericQ] := Block[{$$4187, $$4190, $$4191, $$4194, $$4201, 
      $$4197, $$4206, $$4212, $$4209, $$4198, $$4185, $$4186, $$4188, $$4189, 
      $$4192, $$4193, $$4195, $$4199, $$4202, $$4207, $$4210, $$4213, $$4217, 
      $$4219, $$4243, $$4224, $$4255, $$4226, $$4245, $$4228, $$4196, $$4200, 
      $$4203, $$4204, $$4205, $$4208, $$4211, $$4214, $$4215, $$4216, $$4218, 
      $$4220, $$4221, $$4222, $$4223, $$4225, $$4227, $$4229, $$4230, $$4231, 
      $$4232, $$4249, $$4267, $$4268, $$4269, $$4270, $$4271, $$4272, $$4240, 
      $$4241, $$4242, $$4244, $$4246, $$4247, $$4248, $$4250, $$4251, $$4252, 
      $$4253, $$4254, $$4256, $$4257, $$4258, $$4259, $$4260, $$4261, $$4262, 
      $$4263, $$4264, $$4265, $$4233, $$4234, $$4235, $$4236, $$4237, $$4238, 
      $$4239, $$4273, $$4274, $$4275, $$4276, $$4277, $$4278, $$4279, $$4280, 
      $$4281, $$4282, $$4283, $$4284, $$4285, $$4286, $$4287, $$4288, $$4289, 
      $$4290, $$4291, $$4292, $$4293, $$4294, $$4295, $$4296, $$4297}, 
     $$4187 = -x3; $$4190 = -x4; $$4191 = x1 + $$4190; $$4194 = -y3; 
      $$4201 = -y4; $$4197 = x3^2; $$4206 = x4^2; $$4212 = -y1; 
      $$4209 = x1^2; $$4198 = -y2; $$4185 = -x2; $$4186 = x1 + $$4185; 
      $$4188 = x1 + $$4187; $$4189 = x2 + $$4187; $$4192 = x2 + $$4190; 
      $$4193 = x3 + $$4190; $$4195 = y2 + $$4194; $$4199 = y1 + $$4198; 
      $$4202 = y2 + $$4201; $$4207 = y1 + $$4194; $$4210 = y3 + $$4201; 
      $$4213 = y4 + $$4212; $$4217 = y2 + $$4212; $$4219 = y4 + $$4198; 
      $$4243 = x4^3; $$4224 = y3 + $$4212; $$4255 = x3^3; 
      $$4226 = y1 + $$4201; $$4245 = x1^3; $$4228 = y4 + $$4194; 
      $$4196 = x1*x4*$$4191*$$4195; $$4200 = x4*$$4199; $$4203 = x1*$$4202; 
      $$4204 = $$4200 + $$4203; $$4205 = $$4197*$$4204; 
      $$4208 = $$4206*$$4207; $$4211 = $$4209*$$4210; $$4214 = $$4197*$$4213; 
      $$4215 = $$4208 + $$4211 + $$4214; $$4216 = x2*$$4215; 
      $$4218 = $$4206*$$4217; $$4220 = $$4209*$$4219; 
      $$4221 = $$4218 + $$4220; $$4222 = x3*$$4221; $$4223 = x2^2; 
      $$4225 = x4*$$4224; $$4227 = x3*$$4226; $$4229 = x1*$$4228; 
      $$4230 = $$4225 + $$4227 + $$4229; $$4231 = $$4223*$$4230; 
      $$4232 = $$4196 + $$4205 + $$4216 + $$4222 + $$4231; $$4249 = x2^3; 
      $$4267 = $$4186^(-2); $$4268 = $$4188^(-2); $$4269 = $$4189^(-2); 
      $$4270 = $$4191^(-2); $$4271 = $$4192^(-2); $$4272 = $$4193^(-2); 
      $$4240 = -$$4206; $$4241 = $$4209 + $$4240; 
      $$4242 = -(x1*x4*$$4195*$$4241); $$4244 = $$4199*$$4243; 
      $$4246 = $$4202*$$4245; $$4247 = $$4244 + $$4246; $$4248 = x3*$$4247; 
      $$4250 = x4*$$4207; $$4251 = x1*$$4210; $$4252 = x3*$$4213; 
      $$4253 = $$4250 + $$4251 + $$4252; $$4254 = $$4249*$$4253; 
      $$4256 = x4*$$4217; $$4257 = x1*$$4219; $$4258 = $$4256 + $$4257; 
      $$4259 = $$4255*$$4258; $$4260 = $$4224*$$4243; $$4261 = $$4226*$$4255; 
      $$4262 = $$4228*$$4245; $$4263 = $$4260 + $$4261 + $$4262; 
      $$4264 = x2*$$4263; $$4265 = $$4242 + $$4248 + $$4254 + $$4259 + 
        $$4264; $$4233 = $$4232^(-1); $$4234 = $$4186^(-1); 
      $$4235 = $$4188^(-1); $$4236 = $$4189^(-1); $$4237 = $$4191^(-1); 
      $$4238 = $$4192^(-1); $$4239 = $$4193^(-1); 
      $$4273 = $$4191*$$4195*$$4206*$$4209; $$4274 = $$4199*$$4206; 
      $$4275 = $$4202*$$4209; $$4276 = $$4274 + $$4275; 
      $$4277 = $$4255*$$4276; $$4278 = $$4207*$$4243; $$4279 = $$4210*$$4245; 
      $$4280 = $$4213*$$4255; $$4281 = $$4278 + $$4279 + $$4280; 
      $$4282 = $$4223*$$4281; $$4283 = $$4217*$$4243; $$4284 = $$4219*$$4245; 
      $$4285 = $$4283 + $$4284; $$4286 = $$4197*$$4285; 
      $$4287 = $$4206*$$4224; $$4288 = $$4197*$$4226; $$4289 = $$4209*$$4228; 
      $$4290 = $$4287 + $$4288 + $$4289; $$4291 = $$4249*$$4290; 
      $$4292 = $$4273 + $$4277 + $$4282 + $$4286 + $$4291; 
      $$4293 = -3*$$4232*$$4267*$$4268*$$4269*$$4270*$$4271*$$4272*$$4292; 
      $$4294 = $$4265^2; $$4295 = $$4267*$$4268*$$4269*$$4270*$$4271*$$4272*
        $$4294; $$4296 = $$4293 + $$4295; $$4297 = Sqrt[$$4296]; 
      {-($$4186*$$4188*$$4189*$$4191*$$4192*$$4193*$$4233*
          ($$4234*$$4235*$$4236*$$4237*$$4238*$$4239*$$4265 + $$4297))/3, 
       ($$4186*$$4188*$$4189*$$4191*$$4192*$$4193*$$4233*
         (-($$4234*$$4235*$$4236*$$4237*$$4238*$$4239*$$4265) + $$4297))/3}]
 
Attributes[$$4187] = {Temporary}
 
Attributes[$$4190] = {Temporary}
 
Attributes[$$4191] = {Temporary}
 
Attributes[$$4194] = {Temporary}
 
Attributes[$$4201] = {Temporary}
 
Attributes[$$4197] = {Temporary}
 
Attributes[$$4206] = {Temporary}
 
Attributes[$$4212] = {Temporary}
 
Attributes[$$4209] = {Temporary}
 
Attributes[$$4198] = {Temporary}
 
Attributes[$$4185] = {Temporary}
 
Attributes[$$4186] = {Temporary}
 
Attributes[$$4188] = {Temporary}
 
Attributes[$$4189] = {Temporary}
 
Attributes[$$4192] = {Temporary}
 
Attributes[$$4193] = {Temporary}
 
Attributes[$$4195] = {Temporary}
 
Attributes[$$4199] = {Temporary}
 
Attributes[$$4202] = {Temporary}
 
Attributes[$$4207] = {Temporary}
 
Attributes[$$4210] = {Temporary}
 
Attributes[$$4213] = {Temporary}
 
Attributes[$$4217] = {Temporary}
 
Attributes[$$4219] = {Temporary}
 
Attributes[$$4243] = {Temporary}
 
Attributes[$$4224] = {Temporary}
 
Attributes[$$4255] = {Temporary}
 
Attributes[$$4226] = {Temporary}
 
Attributes[$$4245] = {Temporary}
 
Attributes[$$4228] = {Temporary}
 
Attributes[$$4196] = {Temporary}
 
Attributes[$$4200] = {Temporary}
 
Attributes[$$4203] = {Temporary}
 
Attributes[$$4204] = {Temporary}
 
Attributes[$$4205] = {Temporary}
 
Attributes[$$4208] = {Temporary}
 
Attributes[$$4211] = {Temporary}
 
Attributes[$$4214] = {Temporary}
 
Attributes[$$4215] = {Temporary}
 
Attributes[$$4216] = {Temporary}
 
Attributes[$$4218] = {Temporary}
 
Attributes[$$4220] = {Temporary}
 
Attributes[$$4221] = {Temporary}
 
Attributes[$$4222] = {Temporary}
 
Attributes[$$4223] = {Temporary}
 
Attributes[$$4225] = {Temporary}
 
Attributes[$$4227] = {Temporary}
 
Attributes[$$4229] = {Temporary}
 
Attributes[$$4230] = {Temporary}
 
Attributes[$$4231] = {Temporary}
 
Attributes[$$4232] = {Temporary}
 
Attributes[$$4249] = {Temporary}
 
Attributes[$$4267] = {Temporary}
 
Attributes[$$4268] = {Temporary}
 
Attributes[$$4269] = {Temporary}
 
Attributes[$$4270] = {Temporary}
 
Attributes[$$4271] = {Temporary}
 
Attributes[$$4272] = {Temporary}
 
Attributes[$$4240] = {Temporary}
 
Attributes[$$4241] = {Temporary}
 
Attributes[$$4242] = {Temporary}
 
Attributes[$$4244] = {Temporary}
 
Attributes[$$4246] = {Temporary}
 
Attributes[$$4247] = {Temporary}
 
Attributes[$$4248] = {Temporary}
 
Attributes[$$4250] = {Temporary}
 
Attributes[$$4251] = {Temporary}
 
Attributes[$$4252] = {Temporary}
 
Attributes[$$4253] = {Temporary}
 
Attributes[$$4254] = {Temporary}
 
Attributes[$$4256] = {Temporary}
 
Attributes[$$4257] = {Temporary}
 
Attributes[$$4258] = {Temporary}
 
Attributes[$$4259] = {Temporary}
 
Attributes[$$4260] = {Temporary}
 
Attributes[$$4261] = {Temporary}
 
Attributes[$$4262] = {Temporary}
 
Attributes[$$4263] = {Temporary}
 
Attributes[$$4264] = {Temporary}
 
Attributes[$$4265] = {Temporary}
 
Attributes[$$4233] = {Temporary}
 
Attributes[$$4234] = {Temporary}
 
Attributes[$$4235] = {Temporary}
 
Attributes[$$4236] = {Temporary}
 
Attributes[$$4237] = {Temporary}
 
Attributes[$$4238] = {Temporary}
 
Attributes[$$4239] = {Temporary}
 
Attributes[$$4273] = {Temporary}
 
Attributes[$$4274] = {Temporary}
 
Attributes[$$4275] = {Temporary}
 
Attributes[$$4276] = {Temporary}
 
Attributes[$$4277] = {Temporary}
 
Attributes[$$4278] = {Temporary}
 
Attributes[$$4279] = {Temporary}
 
Attributes[$$4280] = {Temporary}
 
Attributes[$$4281] = {Temporary}
 
Attributes[$$4282] = {Temporary}
 
Attributes[$$4283] = {Temporary}
 
Attributes[$$4284] = {Temporary}
 
Attributes[$$4285] = {Temporary}
 
Attributes[$$4286] = {Temporary}
 
Attributes[$$4287] = {Temporary}
 
Attributes[$$4288] = {Temporary}
 
Attributes[$$4289] = {Temporary}
 
Attributes[$$4290] = {Temporary}
 
Attributes[$$4291] = {Temporary}
 
Attributes[$$4292] = {Temporary}
 
Attributes[$$4293] = {Temporary}
 
Attributes[$$4294] = {Temporary}
 
Attributes[$$4295] = {Temporary}
 
Attributes[$$4296] = {Temporary}
 
Attributes[$$4297] = {Temporary}
