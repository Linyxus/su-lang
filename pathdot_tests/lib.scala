let lib =
  new(x: { Unit: x.unit.type .. x.unit.type } & { unit: x.Unit }) { Unit := x.unit.type } & { unit = x.unit }
in lib
