criticalDomainLocations[(y1_)?NumericQ, (x1_)?NumericQ, (y2_)?NumericQ, 
     (x2_)?NumericQ, (y3_)?NumericQ, (x3_)?NumericQ] := 
    Block[{$$716, $$712, $$708}, $$716 = -y1; $$712 = -y3; $$708 = -y2; 
      {-(x3^2*(y1 + $$708) + x1^2*(y2 + $$712) + x2^2*($$716 + y3))/
        (2*(x3*($$716 + y2) + x2*(y1 + $$712) + x1*($$708 + y3)))}]
 
criticalDomainLocations[(y1_)?NumericQ, (x1_)?NumericQ, (y2_)?NumericQ, 
     (x2_)?NumericQ, (y3_)?NumericQ, (x3_)?NumericQ, (y4_)?NumericQ, 
     (x4_)?NumericQ] := Block[{$$4547, $$4550, $$4551, $$4554, $$4561, 
      $$4557, $$4566, $$4572, $$4569, $$4558, $$4545, $$4546, $$4548, $$4549, 
      $$4552, $$4553, $$4555, $$4559, $$4562, $$4567, $$4570, $$4573, $$4577, 
      $$4579, $$4603, $$4584, $$4615, $$4586, $$4605, $$4588, $$4556, $$4560, 
      $$4563, $$4564, $$4565, $$4568, $$4571, $$4574, $$4575, $$4576, $$4578, 
      $$4580, $$4581, $$4582, $$4583, $$4585, $$4587, $$4589, $$4590, $$4591, 
      $$4592, $$4609, $$4627, $$4628, $$4629, $$4630, $$4631, $$4632, $$4600, 
      $$4601, $$4602, $$4604, $$4606, $$4607, $$4608, $$4610, $$4611, $$4612, 
      $$4613, $$4614, $$4616, $$4617, $$4618, $$4619, $$4620, $$4621, $$4622, 
      $$4623, $$4624, $$4625, $$4593, $$4594, $$4595, $$4596, $$4597, $$4598, 
      $$4599, $$4633, $$4634, $$4635, $$4636, $$4637, $$4638, $$4639, $$4640, 
      $$4641, $$4642, $$4643, $$4644, $$4645, $$4646, $$4647, $$4648, $$4649, 
      $$4650, $$4651, $$4652, $$4653, $$4654, $$4655, $$4656, $$4657}, 
     $$4547 = -x3; $$4550 = -x4; $$4551 = x1 + $$4550; $$4554 = -y3; 
      $$4561 = -y4; $$4557 = x3^2; $$4566 = x4^2; $$4572 = -y1; 
      $$4569 = x1^2; $$4558 = -y2; $$4545 = -x2; $$4546 = x1 + $$4545; 
      $$4548 = x1 + $$4547; $$4549 = x2 + $$4547; $$4552 = x2 + $$4550; 
      $$4553 = x3 + $$4550; $$4555 = y2 + $$4554; $$4559 = y1 + $$4558; 
      $$4562 = y2 + $$4561; $$4567 = y1 + $$4554; $$4570 = y3 + $$4561; 
      $$4573 = $$4572 + y4; $$4577 = $$4572 + y2; $$4579 = $$4558 + y4; 
      $$4603 = x4^3; $$4584 = $$4572 + y3; $$4615 = x3^3; 
      $$4586 = y1 + $$4561; $$4605 = x1^3; $$4588 = $$4554 + y4; 
      $$4556 = x1*$$4551*x4*$$4555; $$4560 = x4*$$4559; $$4563 = x1*$$4562; 
      $$4564 = $$4560 + $$4563; $$4565 = $$4557*$$4564; 
      $$4568 = $$4566*$$4567; $$4571 = $$4569*$$4570; $$4574 = $$4557*$$4573; 
      $$4575 = $$4568 + $$4571 + $$4574; $$4576 = x2*$$4575; 
      $$4578 = $$4566*$$4577; $$4580 = $$4569*$$4579; 
      $$4581 = $$4578 + $$4580; $$4582 = x3*$$4581; $$4583 = x2^2; 
      $$4585 = x4*$$4584; $$4587 = x3*$$4586; $$4589 = x1*$$4588; 
      $$4590 = $$4585 + $$4587 + $$4589; $$4591 = $$4583*$$4590; 
      $$4592 = $$4556 + $$4565 + $$4576 + $$4582 + $$4591; $$4609 = x2^3; 
      $$4627 = $$4546^(-2); $$4628 = $$4548^(-2); $$4629 = $$4549^(-2); 
      $$4630 = $$4551^(-2); $$4631 = $$4552^(-2); $$4632 = $$4553^(-2); 
      $$4600 = -$$4566; $$4601 = $$4569 + $$4600; 
      $$4602 = -(x1*x4*$$4601*$$4555); $$4604 = $$4603*$$4559; 
      $$4606 = $$4605*$$4562; $$4607 = $$4604 + $$4606; $$4608 = x3*$$4607; 
      $$4610 = x4*$$4567; $$4611 = x1*$$4570; $$4612 = x3*$$4573; 
      $$4613 = $$4610 + $$4611 + $$4612; $$4614 = $$4609*$$4613; 
      $$4616 = x4*$$4577; $$4617 = x1*$$4579; $$4618 = $$4616 + $$4617; 
      $$4619 = $$4615*$$4618; $$4620 = $$4603*$$4584; $$4621 = $$4615*$$4586; 
      $$4622 = $$4605*$$4588; $$4623 = $$4620 + $$4621 + $$4622; 
      $$4624 = x2*$$4623; $$4625 = $$4602 + $$4608 + $$4614 + $$4619 + 
        $$4624; $$4593 = $$4592^(-1); $$4594 = $$4546^(-1); 
      $$4595 = $$4548^(-1); $$4596 = $$4549^(-1); $$4597 = $$4551^(-1); 
      $$4598 = $$4552^(-1); $$4599 = $$4553^(-1); 
      $$4633 = $$4569*$$4551*$$4566*$$4555; $$4634 = $$4566*$$4559; 
      $$4635 = $$4569*$$4562; $$4636 = $$4634 + $$4635; 
      $$4637 = $$4615*$$4636; $$4638 = $$4603*$$4567; $$4639 = $$4605*$$4570; 
      $$4640 = $$4615*$$4573; $$4641 = $$4638 + $$4639 + $$4640; 
      $$4642 = $$4583*$$4641; $$4643 = $$4603*$$4577; $$4644 = $$4605*$$4579; 
      $$4645 = $$4643 + $$4644; $$4646 = $$4557*$$4645; 
      $$4647 = $$4566*$$4584; $$4648 = $$4557*$$4586; $$4649 = $$4569*$$4588; 
      $$4650 = $$4647 + $$4648 + $$4649; $$4651 = $$4609*$$4650; 
      $$4652 = $$4633 + $$4637 + $$4642 + $$4646 + $$4651; 
      $$4653 = -3*$$4627*$$4628*$$4629*$$4630*$$4631*$$4632*$$4592*$$4652; 
      $$4654 = $$4625^2; $$4655 = $$4627*$$4628*$$4629*$$4630*$$4631*$$4632*
        $$4654; $$4656 = $$4653 + $$4655; $$4657 = Sqrt[$$4656]; 
      {-($$4546*$$4548*$$4549*$$4551*$$4552*$$4553*$$4593*
          ($$4594*$$4595*$$4596*$$4597*$$4598*$$4599*$$4625 + $$4657))/3, 
       ($$4546*$$4548*$$4549*$$4551*$$4552*$$4553*$$4593*
         (-($$4594*$$4595*$$4596*$$4597*$$4598*$$4599*$$4625) + $$4657))/3}]
 
Attributes[$$716] = {Temporary}
 
Attributes[$$712] = {Temporary}
 
Attributes[$$708] = {Temporary}
 
Attributes[$$4547] = {Temporary}
 
Attributes[$$4550] = {Temporary}
 
Attributes[$$4551] = {Temporary}
 
Attributes[$$4554] = {Temporary}
 
Attributes[$$4561] = {Temporary}
 
Attributes[$$4557] = {Temporary}
 
Attributes[$$4566] = {Temporary}
 
Attributes[$$4572] = {Temporary}
 
Attributes[$$4569] = {Temporary}
 
Attributes[$$4558] = {Temporary}
 
Attributes[$$4545] = {Temporary}
 
Attributes[$$4546] = {Temporary}
 
Attributes[$$4548] = {Temporary}
 
Attributes[$$4549] = {Temporary}
 
Attributes[$$4552] = {Temporary}
 
Attributes[$$4553] = {Temporary}
 
Attributes[$$4555] = {Temporary}
 
Attributes[$$4559] = {Temporary}
 
Attributes[$$4562] = {Temporary}
 
Attributes[$$4567] = {Temporary}
 
Attributes[$$4570] = {Temporary}
 
Attributes[$$4573] = {Temporary}
 
Attributes[$$4577] = {Temporary}
 
Attributes[$$4579] = {Temporary}
 
Attributes[$$4603] = {Temporary}
 
Attributes[$$4584] = {Temporary}
 
Attributes[$$4615] = {Temporary}
 
Attributes[$$4586] = {Temporary}
 
Attributes[$$4605] = {Temporary}
 
Attributes[$$4588] = {Temporary}
 
Attributes[$$4556] = {Temporary}
 
Attributes[$$4560] = {Temporary}
 
Attributes[$$4563] = {Temporary}
 
Attributes[$$4564] = {Temporary}
 
Attributes[$$4565] = {Temporary}
 
Attributes[$$4568] = {Temporary}
 
Attributes[$$4571] = {Temporary}
 
Attributes[$$4574] = {Temporary}
 
Attributes[$$4575] = {Temporary}
 
Attributes[$$4576] = {Temporary}
 
Attributes[$$4578] = {Temporary}
 
Attributes[$$4580] = {Temporary}
 
Attributes[$$4581] = {Temporary}
 
Attributes[$$4582] = {Temporary}
 
Attributes[$$4583] = {Temporary}
 
Attributes[$$4585] = {Temporary}
 
Attributes[$$4587] = {Temporary}
 
Attributes[$$4589] = {Temporary}
 
Attributes[$$4590] = {Temporary}
 
Attributes[$$4591] = {Temporary}
 
Attributes[$$4592] = {Temporary}
 
Attributes[$$4609] = {Temporary}
 
Attributes[$$4627] = {Temporary}
 
Attributes[$$4628] = {Temporary}
 
Attributes[$$4629] = {Temporary}
 
Attributes[$$4630] = {Temporary}
 
Attributes[$$4631] = {Temporary}
 
Attributes[$$4632] = {Temporary}
 
Attributes[$$4600] = {Temporary}
 
Attributes[$$4601] = {Temporary}
 
Attributes[$$4602] = {Temporary}
 
Attributes[$$4604] = {Temporary}
 
Attributes[$$4606] = {Temporary}
 
Attributes[$$4607] = {Temporary}
 
Attributes[$$4608] = {Temporary}
 
Attributes[$$4610] = {Temporary}
 
Attributes[$$4611] = {Temporary}
 
Attributes[$$4612] = {Temporary}
 
Attributes[$$4613] = {Temporary}
 
Attributes[$$4614] = {Temporary}
 
Attributes[$$4616] = {Temporary}
 
Attributes[$$4617] = {Temporary}
 
Attributes[$$4618] = {Temporary}
 
Attributes[$$4619] = {Temporary}
 
Attributes[$$4620] = {Temporary}
 
Attributes[$$4621] = {Temporary}
 
Attributes[$$4622] = {Temporary}
 
Attributes[$$4623] = {Temporary}
 
Attributes[$$4624] = {Temporary}
 
Attributes[$$4625] = {Temporary}
 
Attributes[$$4593] = {Temporary}
 
Attributes[$$4594] = {Temporary}
 
Attributes[$$4595] = {Temporary}
 
Attributes[$$4596] = {Temporary}
 
Attributes[$$4597] = {Temporary}
 
Attributes[$$4598] = {Temporary}
 
Attributes[$$4599] = {Temporary}
 
Attributes[$$4633] = {Temporary}
 
Attributes[$$4634] = {Temporary}
 
Attributes[$$4635] = {Temporary}
 
Attributes[$$4636] = {Temporary}
 
Attributes[$$4637] = {Temporary}
 
Attributes[$$4638] = {Temporary}
 
Attributes[$$4639] = {Temporary}
 
Attributes[$$4640] = {Temporary}
 
Attributes[$$4641] = {Temporary}
 
Attributes[$$4642] = {Temporary}
 
Attributes[$$4643] = {Temporary}
 
Attributes[$$4644] = {Temporary}
 
Attributes[$$4645] = {Temporary}
 
Attributes[$$4646] = {Temporary}
 
Attributes[$$4647] = {Temporary}
 
Attributes[$$4648] = {Temporary}
 
Attributes[$$4649] = {Temporary}
 
Attributes[$$4650] = {Temporary}
 
Attributes[$$4651] = {Temporary}
 
Attributes[$$4652] = {Temporary}
 
Attributes[$$4653] = {Temporary}
 
Attributes[$$4654] = {Temporary}
 
Attributes[$$4655] = {Temporary}
 
Attributes[$$4656] = {Temporary}
 
Attributes[$$4657] = {Temporary}
