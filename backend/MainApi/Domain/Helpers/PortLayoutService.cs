using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Docks;
using DDDSample1.Infrastructure.Docks;
using DDDSample1.Infrastructure.StorageAreas;
using DDDNetCore.Domain.PortInfrastructure.StorageArea; 

public class PortLayoutService
{
    private readonly IDockRepository _dockRepo;
    private readonly IStorageAreaRepository _storageRepo;
    
    private const double DOCK_GAP = 15;
    private const double COL_GAP = 5;
    private const double ROW_GAP = 5;
    private const double MARGIN_SIDE = 13;
    private const double ROAD_FRONT = 40;
    private const double ZONE_GAP = 20;
    
    private const double DOCK_WIDTH = 30;
    private const double DOCK_DEPTH = 10;
    private const double YARD_WIDTH = 60;
    private const double YARD_DEPTH = 40;
    private const double WH_WIDTH = 40;
    private const double WH_DEPTH = 25;
    
    private const double DOCK_WIDTH_MIN = 15;
    private const double DOCK_WIDTH_MAX = 50;
    private const double DOCK_DEP_MIN = 5;
    private const double DOCK_DEP_MAX = 25;
    private const double YARD_VISUAL_WIDTH_MIN = 49;
    private const double YARD_VISUAL_WIDTH_MAX = 71;
    private const double YARD_VISUAL_DEPTH_MIN = 34;
    private const double YARD_VISUAL_DEPTH_MAX = 49;
    private const double WH_VISUAL_WIDTH_MIN = 32;
    private const double WH_VISUAL_WIDTH_MAX = 49;
    private const double WH_VISUAL_DEPTH_MIN = 21;
    private const double WH_VISUAL_DEPTH_MAX = 32;
    // =========================================

    public PortLayoutService(IDockRepository dockRepo, IStorageAreaRepository storageRepo)
    {
        _dockRepo = dockRepo;
        _storageRepo = storageRepo;
    }
    
    private double Clamp(double value, double min, double max)
    {
        return Math.Max(min, Math.Min(max, value));
    }

    public async Task<object> GeneratePortLayoutAsync()
    {
        var docksFromDb = await _dockRepo.GetAllAsync();
        var storageAreasFromDb = await _storageRepo.GetAllAsync();

        var warehouses = storageAreasFromDb.Where(s => s.StorageAreaType == StorageAreaType.Warehouse).ToList();
        var yards = storageAreasFromDb.Where(s => s.StorageAreaType == StorageAreaType.Yard).ToList();
        
        var adjustedDocks = new List<LayoutElement>();
        var adjustedWarehouses = new List<LayoutElement>();
        var adjustedYards = new List<LayoutElement>();
        
        docksFromDb.ForEach((dock) =>
        {
            double width = Clamp(dock.Length, DOCK_WIDTH_MIN, DOCK_WIDTH_MAX);
            double depth = Clamp(dock.Depth, DOCK_DEP_MIN, DOCK_DEP_MAX);

            adjustedDocks.Add(new LayoutElement
            {
                Type = "dock",
                IdPlaceholder = dock.Id.AsString(),
                Width = width,
                Depth = depth
            });
        });
        
        warehouses.ForEach((wh) =>
        {
            double width = Clamp(wh.Width, WH_VISUAL_WIDTH_MIN, WH_VISUAL_WIDTH_MAX);
            double depth = Clamp(wh.Length, WH_VISUAL_DEPTH_MIN, WH_VISUAL_DEPTH_MAX);

            adjustedWarehouses.Add(new LayoutElement
            {
                Type = "storage_area",
                Subtype = "warehouse",
                IdPlaceholder = wh.Id.AsString(),
                Width = width,
                Depth = depth
            });
        });
        
        yards.ForEach((yd) =>
        {
            double width = Clamp(yd.Width, YARD_VISUAL_WIDTH_MIN, YARD_VISUAL_WIDTH_MAX);
            double depth = Clamp(yd.Length, YARD_VISUAL_DEPTH_MIN, YARD_VISUAL_DEPTH_MAX);

            adjustedYards.Add(new LayoutElement
            {
                Type = "storage_area",
                Subtype = "yard",
                IdPlaceholder = yd.Id.AsString(),
                Width = width,
                Depth = depth
            });
        });
        
        var numWH = adjustedWarehouses.Count;
        var numYD = adjustedYards.Count;
        var numDocks = adjustedDocks.Count;
        
        var colsWH = numWH > 0 ? (int)Math.Round(Math.Sqrt(numWH)) : 0;
        var colsYD = numYD > 0 ? (int)Math.Round(Math.Sqrt(numYD)) : 0;

        var finalColsWH = (numWH > 0 && colsWH == 0) ? 1 : colsWH;
        var finalColsYD = (numYD > 0 && colsYD == 0) ? 1 : colsYD;
        
        double widthBlockWH = 0;
        if (numWH > 0)
        {
            var colWidths = new double[finalColsWH];
            for (int i = 0; i < numWH; i++)
            {
                var colIndex = i % finalColsWH;
                colWidths[colIndex] = Math.Max(colWidths[colIndex], adjustedWarehouses[i].Width);
            }
            widthBlockWH = colWidths.Sum() + (finalColsWH - 1) * COL_GAP;
        }

        double widthBlockYD = 0;
        if (numYD > 0)
        {
            var colWidths = new double[finalColsYD];
            for (int i = 0; i < numYD; i++)
            {
                var colIndex = i % finalColsYD;
                colWidths[colIndex] = Math.Max(colWidths[colIndex], adjustedYards[i].Width);
            }
            widthBlockYD = colWidths.Sum() + (finalColsYD - 1) * COL_GAP;
        }

        double buildingsTotalWidth = widthBlockWH + widthBlockYD;
        if (numWH > 0 && numYD > 0)
        {
            buildingsTotalWidth += ZONE_GAP;
        }
        
        double docksTotalWidth = adjustedDocks.Sum(d => d.Width) + (numDocks > 0 ? (numDocks - 1) * DOCK_GAP : 0);

        double portWidth = Math.Max(docksTotalWidth, buildingsTotalWidth) + (MARGIN_SIDE * 2);
        portWidth = Math.Ceiling(portWidth / 10) * 10;

        var rowsWH = finalColsWH > 0 ? (int)Math.Ceiling((double)numWH / finalColsWH) : 0;
        var rowsYD = finalColsYD > 0 ? (int)Math.Ceiling((double)numYD / finalColsYD) : 0;

        double depthBlockWH = 0;
        if (numWH > 0)
        {
            var rowDepths = new double[rowsWH];
            for (int i = 0; i < numWH; i++)
            {
                var rowIndex = (int)Math.Floor((double)i / finalColsWH);
                rowDepths[rowIndex] = Math.Max(rowDepths[rowIndex], adjustedWarehouses[i].Depth);
            }
            depthBlockWH = rowDepths.Sum() + (rowsWH > 0 ? (rowsWH - 1) * ROW_GAP : 0);
        }

        double depthBlockYD = 0;
        if (numYD > 0)
        {
            var rowDepths = new double[rowsYD];
            for (int i = 0; i < numYD; i++)
            {
                var rowIndex = (int)Math.Floor((double)i / finalColsYD);
                rowDepths[rowIndex] = Math.Max(rowDepths[rowIndex], adjustedYards[i].Depth);
            }
            depthBlockYD = rowDepths.Sum() + (rowsYD > 0 ? (rowsYD - 1) * (ROW_GAP + 4) : 0); 
        }

        double maxBuildingDepth = Math.Max(depthBlockWH, depthBlockYD);
        double physicalDepth = ROAD_FRONT + maxBuildingDepth + MARGIN_SIDE;

        double squaredDepth = portWidth * 0.6;

        double portDepth = Math.Max(physicalDepth, squaredDepth);

        portDepth = Math.Max(portDepth, 140); 
        portDepth = Math.Ceiling(portDepth / 10) * 10; 
        
        var layoutElements = new List<LayoutElement>();
 
        double totalDocksWidth = adjustedDocks.Sum(d => d.Width);
        double startDocksX = -(docksTotalWidth / 2);
        double currentDockX = startDocksX;

        adjustedDocks.ForEach((dock, i) =>
        {
            double x_old = currentDockX + (dock.Width / 2);
            double z_old = -(dock.Depth / 2);

            dock.PositionX_Old = x_old;
            dock.PositionZ_Old = z_old;
            layoutElements.Add(dock);

            currentDockX += dock.Width + DOCK_GAP;
        });

        double backLimitZ = portDepth - MARGIN_SIDE;

        double totalContentWidth = buildingsTotalWidth;
        double currentX = -(totalContentWidth / 2); 

        if (numWH > 0)
        {
            double startZ_WH = backLimitZ - depthBlockWH + (adjustedWarehouses[0].Depth / 2);
            
            var colWidths = new double[finalColsWH];
            for (int i = 0; i < numWH; i++)
            {
                var colIndex = i % finalColsWH;
                colWidths[colIndex] = Math.Max(colWidths[colIndex], adjustedWarehouses[i].Width);
            }

            double currentColX = currentX;
            double[] currentYOffset = new double[rowsWH];

            for (int i = 0; i < numWH; i++)
            {
                var wh = adjustedWarehouses[i];
                var colIndex = i % finalColsWH;
                var rowIndex = (int)Math.Floor((double)i / finalColsWH);

                if (colIndex == 0)
                {
                    currentColX = currentX;
                    if (rowIndex > 0)
                    {
                        currentYOffset[rowIndex] = currentYOffset[rowIndex - 1] + 
                                                   adjustedWarehouses.Where((w, idx) => (int)Math.Floor((double)idx / finalColsWH) == rowIndex - 1)
                                                                     .Max(w => w.Depth) + ROW_GAP;
                    }
                }
                
                double x_old = currentColX + (wh.Width / 2);
                double z_old = backLimitZ - wh.Depth / 2 - currentYOffset[rowIndex];

                wh.PositionX_Old = x_old;
                wh.PositionZ_Old = z_old;
                layoutElements.Add(wh);

                currentColX += colWidths[colIndex] + COL_GAP;
            }
            
            currentX += widthBlockWH + ZONE_GAP;
        }
        
        if (numYD > 0)
        {
            double startZ_YD = backLimitZ - depthBlockYD + (adjustedYards[0].Depth / 2);
            
            var colWidths = new double[finalColsYD];
            for (int i = 0; i < numYD; i++)
            {
                var colIndex = i % finalColsYD;
                colWidths[colIndex] = Math.Max(colWidths[colIndex], adjustedYards[i].Width);
            }

            double currentColX = currentX;
            double[] currentYOffset = new double[rowsYD];

            for (int i = 0; i < numYD; i++)
            {
                var yd = adjustedYards[i];
                var colIndex = i % finalColsYD;
                var rowIndex = (int)Math.Floor((double)i / finalColsYD);
                
                if (colIndex == 0)
                {
                    currentColX = currentX;
                    if (rowIndex > 0)
                    {
                        currentYOffset[rowIndex] = currentYOffset[rowIndex - 1] + 
                                                   adjustedYards.Where((y, idx) => (int)Math.Floor((double)idx / finalColsYD) == rowIndex - 1)
                                                                 .Max(y => y.Depth) + ROW_GAP + 4; 
                    }
                }
                
                double x_old = currentColX + (yd.Width / 2);
                double z_old = backLimitZ - yd.Depth / 2 - currentYOffset[rowIndex];

                yd.PositionX_Old = x_old;
                yd.PositionZ_Old = z_old;
                layoutElements.Add(yd);

                currentColX += colWidths[colIndex] + COL_GAP;
            }
        }
        
        double Z_OFFSET_SCENE_CENTER = portDepth / 2;
        
        var elements = new List<object>();
        foreach (var el in layoutElements)
        {
            double x_new = el.PositionX_Old; 
            double z_new = el.PositionZ_Old - Z_OFFSET_SCENE_CENTER; 
            
            elements.Add(new
            {
                type = el.Type,
                subtype = el.Subtype,
                id_placeholder = el.IdPlaceholder,
                position = new { x = Math.Round(x_new, 1), z = Math.Round(z_new, 1) }, 
                size = new { width = Math.Round(el.Width, 1), depth = Math.Round(el.Depth, 1) }
            });
        }

        var result = new
        {
            port_dimensions = new
            {
                width = Math.Round(portWidth, 1),
                depth = Math.Round(portDepth, 1)
            },
            elements
        };

        return result;
    }
}
public class LayoutElement
{
    public string Type { get; set; }
    public string Subtype { get; set; }
    public string IdPlaceholder { get; set; }
    public double PositionX_Old { get; set; }
    public double PositionZ_Old { get; set; }
    public double Width { get; set; } 
    public double Depth { get; set; } 
}

public static class ListExtensions
{
    public static void ForEach<T>(this List<T> list, Action<T, int> action)
    {
        for (int i = 0; i < list.Count; i++)
        {
            action(list[i], i);
        }
    }
}