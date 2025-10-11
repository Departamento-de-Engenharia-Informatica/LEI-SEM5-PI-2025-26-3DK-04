using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Docks;

public class DockDto
{
    public string Name { get; set; }
    public double Length { get; set; }
    public double Depth { get; set; }
    public int MaxDraft { get; set; }
    public string Coordinates { get; set; }
    public string LocationDescription { get; set; }
    public List<Guid> VesselTypeIds { get; set; }
}
