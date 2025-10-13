using System;
using System.Collections.Generic;

namespace DDDNetCore.Infraestructure.Docks;

public class DockDetailsDto
{
    public Guid Id { get; set; }
    public string Name { get; set; }
    public double Length { get; set; }
    public double Depth { get; set; }
    public int MaxDraft { get; set; }
    public string Coordinates { get; set; }
    public string LocationDescription { get; set; }
    public List<string> AllowedVesselTypes { get; set; }
}
