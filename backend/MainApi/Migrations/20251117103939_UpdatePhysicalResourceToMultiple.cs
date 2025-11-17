using System;
using Microsoft.EntityFrameworkCore.Migrations;
using Npgsql.EntityFrameworkCore.PostgreSQL.Metadata;

#nullable disable

namespace DDDNetCore.Migrations
{
    /// <inheritdoc />
    public partial class UpdatePhysicalResourceToMultiple : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Docks",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    Name = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: false),
                    Length = table.Column<double>(type: "double precision", nullable: false),
                    Depth = table.Column<double>(type: "double precision", nullable: false),
                    MaxDraft = table.Column<int>(type: "integer", nullable: false),
                    Location_Coordinates = table.Column<string>(type: "text", nullable: true),
                    Location_Description = table.Column<string>(type: "text", nullable: true),
                    Active = table.Column<bool>(type: "boolean", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Docks", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Organizations",
                columns: table => new
                {
                    Id = table.Column<string>(type: "character varying(10)", maxLength: 10, nullable: false),
                    LegalName = table.Column<string>(type: "character varying(200)", maxLength: 200, nullable: false),
                    AlternativeName = table.Column<string>(type: "character varying(200)", maxLength: 200, nullable: true),
                    Address = table.Column<string>(type: "character varying(500)", maxLength: 500, nullable: false),
                    TaxNumber = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Organizations", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "PhysicalResources",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    Description = table.Column<string>(type: "text", nullable: false),
                    Type = table.Column<string>(type: "text", nullable: false),
                    Capacity = table.Column<double>(type: "double precision", nullable: false),
                    AssignedArea = table.Column<string>(type: "text", nullable: true),
                    SetupTime = table.Column<int>(type: "integer", nullable: true),
                    Status = table.Column<string>(type: "text", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PhysicalResources", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Qualifications",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    Name = table.Column<string>(type: "character varying(150)", maxLength: 150, nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Qualifications", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "StaffMembers",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    Name = table.Column<string>(type: "character varying(200)", maxLength: 200, nullable: false),
                    Email = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: false),
                    PhoneNumber = table.Column<int>(type: "integer", nullable: false),
                    OperationalWindow = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: true),
                    Status = table.Column<string>(type: "text", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StaffMembers", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "StorageAreas",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    Code = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false),
                    Designation = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: false),
                    StorageAreaType = table.Column<int>(type: "integer", nullable: false),
                    Location_Coordinates = table.Column<string>(type: "text", nullable: true),
                    Location_Description = table.Column<string>(type: "text", nullable: true),
                    MaxCapacityTEUs = table.Column<int>(type: "integer", nullable: false),
                    CurrentOccupancyTEUs = table.Column<int>(type: "integer", nullable: false),
                    Active = table.Column<bool>(type: "boolean", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StorageAreas", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "UserActivations",
                columns: table => new
                {
                    Id = table.Column<string>(type: "text", nullable: false),
                    UserEmail = table.Column<string>(type: "text", nullable: true),
                    Token = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "timestamp with time zone", nullable: false),
                    ExpiresAt = table.Column<DateTime>(type: "timestamp with time zone", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_UserActivations", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Users",
                columns: table => new
                {
                    Id = table.Column<string>(type: "text", nullable: false),
                    Name = table.Column<string>(type: "text", nullable: true),
                    Picture = table.Column<string>(type: "text", nullable: true),
                    Role = table.Column<int>(type: "integer", nullable: false),
                    Status = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Users", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Vessels",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    ImoNumber = table.Column<string>(type: "character varying(15)", maxLength: 15, nullable: false),
                    Name = table.Column<string>(type: "character varying(200)", maxLength: 200, nullable: false),
                    VesselTypeId = table.Column<Guid>(type: "uuid", nullable: false),
                    Owner = table.Column<string>(type: "character varying(200)", maxLength: 200, nullable: false),
                    Operator = table.Column<string>(type: "character varying(200)", maxLength: 200, nullable: false),
                    Active = table.Column<bool>(type: "boolean", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Vessels", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "VesselTypes",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    Name = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: false),
                    Description = table.Column<string>(type: "character varying(500)", maxLength: 500, nullable: false),
                    Capacity = table.Column<int>(type: "integer", nullable: false),
                    MaxRows = table.Column<int>(type: "integer", nullable: false),
                    MaxBays = table.Column<int>(type: "integer", nullable: false),
                    MaxTiers = table.Column<int>(type: "integer", nullable: false),
                    Active = table.Column<bool>(type: "boolean", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_VesselTypes", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Representatives",
                columns: table => new
                {
                    Id = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false),
                    Name = table.Column<string>(type: "character varying(150)", maxLength: 150, nullable: false),
                    Nationality = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false),
                    Email = table.Column<string>(type: "character varying(150)", maxLength: 150, nullable: false),
                    PhoneNumber = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false),
                    OrganizationId = table.Column<string>(type: "character varying(10)", maxLength: 10, nullable: false),
                    Status = table.Column<string>(type: "text", nullable: false),
                    role = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Representatives", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Representatives_Organizations_OrganizationId",
                        column: x => x.OrganizationId,
                        principalTable: "Organizations",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "PhysicalResourceQualifications",
                columns: table => new
                {
                    PhysicalResourceId = table.Column<Guid>(type: "uuid", nullable: false),
                    QualificationId = table.Column<Guid>(type: "uuid", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PhysicalResourceQualifications", x => new { x.PhysicalResourceId, x.QualificationId });
                    table.ForeignKey(
                        name: "FK_PhysicalResourceQualifications_PhysicalResources_PhysicalRe~",
                        column: x => x.PhysicalResourceId,
                        principalTable: "PhysicalResources",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_PhysicalResourceQualifications_Qualifications_Qualification~",
                        column: x => x.QualificationId,
                        principalTable: "Qualifications",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "StaffMemberQualifications",
                columns: table => new
                {
                    StaffMemberId = table.Column<Guid>(type: "uuid", nullable: false),
                    QualificationId = table.Column<Guid>(type: "uuid", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StaffMemberQualifications", x => new { x.StaffMemberId, x.QualificationId });
                    table.ForeignKey(
                        name: "FK_StaffMemberQualifications_Qualifications_QualificationId",
                        column: x => x.QualificationId,
                        principalTable: "Qualifications",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_StaffMemberQualifications_StaffMembers_StaffMemberId",
                        column: x => x.StaffMemberId,
                        principalTable: "StaffMembers",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "StorageDockAssignments",
                columns: table => new
                {
                    DockId = table.Column<Guid>(type: "uuid", nullable: false),
                    StorageAreaId = table.Column<Guid>(type: "uuid", nullable: false),
                    DistanceMeters = table.Column<double>(type: "double precision", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StorageDockAssignments", x => new { x.StorageAreaId, x.DockId });
                    table.ForeignKey(
                        name: "FK_StorageDockAssignments_StorageAreas_StorageAreaId",
                        column: x => x.StorageAreaId,
                        principalTable: "StorageAreas",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "CrewMember",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Name = table.Column<string>(type: "character varying(150)", maxLength: 150, nullable: false),
                    CitizenId = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false),
                    Nationality = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false),
                    VesselId = table.Column<Guid>(type: "uuid", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_CrewMember", x => x.Id);
                    table.ForeignKey(
                        name: "FK_CrewMember_Vessels_VesselId",
                        column: x => x.VesselId,
                        principalTable: "Vessels",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "VesselVisitNotifications",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    VesselId = table.Column<Guid>(type: "uuid", nullable: false),
                    RejectedReason = table.Column<string>(type: "character varying(500)", maxLength: 500, nullable: true),
                    DecisionTimeStamp = table.Column<DateTime>(type: "timestamp with time zone", nullable: true),
                    DecisionOutcome = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: true),
                    Status = table.Column<string>(type: "text", nullable: false),
                    AssignedDock = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: true),
                    OfficerId = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: true),
                    RepresentativeId = table.Column<string>(type: "text", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "timestamp with time zone", nullable: false),
                    ArrivalTime = table.Column<DateTime>(type: "timestamp with time zone", nullable: true),
                    DepartureTime = table.Column<DateTime>(type: "timestamp with time zone", nullable: true),
                    UnloadTime = table.Column<int>(type: "integer", nullable: true),
                    LoadTime = table.Column<int>(type: "integer", nullable: true),
                    StaffMemberIds = table.Column<string>(type: "character varying(1000)", maxLength: 1000, nullable: true),
                    PhysicalResourceIds = table.Column<string>(type: "character varying(1000)", maxLength: 1000, nullable: true),
                    DockId = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_VesselVisitNotifications", x => x.Id);
                    table.ForeignKey(
                        name: "FK_VesselVisitNotifications_Vessels_VesselId",
                        column: x => x.VesselId,
                        principalTable: "Vessels",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "DockVesselTypes",
                columns: table => new
                {
                    AllowedVesselTypesId = table.Column<Guid>(type: "uuid", nullable: false),
                    DockId = table.Column<Guid>(type: "uuid", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DockVesselTypes", x => new { x.AllowedVesselTypesId, x.DockId });
                    table.ForeignKey(
                        name: "FK_DockVesselTypes_Docks_DockId",
                        column: x => x.DockId,
                        principalTable: "Docks",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_DockVesselTypes_VesselTypes_AllowedVesselTypesId",
                        column: x => x.AllowedVesselTypesId,
                        principalTable: "VesselTypes",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "LoadingCargoManifests",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    VesselVisitNotificationId = table.Column<Guid>(type: "uuid", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_LoadingCargoManifests", x => x.Id);
                    table.ForeignKey(
                        name: "FK_LoadingCargoManifests_VesselVisitNotifications_VesselVisitN~",
                        column: x => x.VesselVisitNotificationId,
                        principalTable: "VesselVisitNotifications",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "UnloadingCargoManifests",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    VesselVisitNotificationId = table.Column<Guid>(type: "uuid", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_UnloadingCargoManifests", x => x.Id);
                    table.ForeignKey(
                        name: "FK_UnloadingCargoManifests_VesselVisitNotifications_VesselVisi~",
                        column: x => x.VesselVisitNotificationId,
                        principalTable: "VesselVisitNotifications",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "LoadingManifestContainers",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    PayloadWeight = table.Column<double>(type: "double precision", nullable: false),
                    ContentsDescription = table.Column<string>(type: "text", nullable: true),
                    CargoManifestId = table.Column<Guid>(type: "uuid", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_LoadingManifestContainers", x => x.Id);
                    table.ForeignKey(
                        name: "FK_LoadingManifestContainers_LoadingCargoManifests_CargoManife~",
                        column: x => x.CargoManifestId,
                        principalTable: "LoadingCargoManifests",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "UnloadingManifestContainers",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uuid", nullable: false),
                    PayloadWeight = table.Column<double>(type: "double precision", nullable: false),
                    ContentsDescription = table.Column<string>(type: "text", nullable: true),
                    CargoManifestId = table.Column<Guid>(type: "uuid", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_UnloadingManifestContainers", x => x.Id);
                    table.ForeignKey(
                        name: "FK_UnloadingManifestContainers_UnloadingCargoManifests_CargoMa~",
                        column: x => x.CargoManifestId,
                        principalTable: "UnloadingCargoManifests",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
            });
            // Organização
    var orgId1 = "ORG-001"; // PK é string(10)
    
    // Qualificações
    var qualIdOperadorGuindaste = Guid.Parse("11110000-0000-0000-0000-000000000001");
    var qualIdCargaPerigosa = Guid.Parse("11110000-0000-0000-0000-000000000002");
    
    // Staff
    var staffIdJoao = Guid.Parse("22220000-0000-0000-0000-000000000001");
    
    // Recursos Físicos
    var resourceIdGuindasteA = Guid.Parse("33330000-0000-0000-0000-000000000001");
    
    // Tipo de Navio
    var vesselTypeIdPanamax = Guid.Parse("44440000-0000-0000-0000-000000000001");
    
    // Navio
    var vesselIdLisboaVoyager = Guid.Parse("55550000-0000-0000-0000-000000000001");
    
    // Docas
    var dockIdA = Guid.Parse("66660000-0000-0000-0000-000000000001");
    var dockIdB = Guid.Parse("66660000-0000-0000-0000-000000000002");
    
    // Áreas de Armazenamento
    var storageIdA1 = Guid.Parse("77770000-0000-0000-0000-000000000001");
    
    // Representante
    var repIdMaria = "REP-001"; // PK é string(50)
    
    // Visita
    var visitId1 = Guid.Parse("88880000-0000-0000-0000-000000000001");
    
    // Manifestos
    var loadingManifestId = Guid.Parse("99990000-0000-0000-0000-000000000001");
    var unloadingManifestId = Guid.Parse("99990000-0000-0000-0000-000000000002");
    
    // User
    var userIdAdmin = "admin@exemplo.com";


    // --- Inserir Dados ---

    // Docks (O seu exemplo, com IDs estáticos)
    migrationBuilder.InsertData(
        table: "Docks",
        columns: new[] { "Id", "Name", "Length", "Depth", "MaxDraft", "Active", "Location_Coordinates", "Location_Description" },
        values: new object[,]
        {
            { dockIdA, "Dock A", 300.0, 12.5, 14, true, "41.145, -8.61", "Cais Principal" },
            { dockIdB, "Dock B", 250.0, 10.2, 12, true, "41.146, -8.62", "Cais Secundário" }
        }
    );

    // Organizations
    migrationBuilder.InsertData(
        table: "Organizations",
        columns: new[] { "Id", "LegalName", "Address", "TaxNumber", "AlternativeName" },
        values: new object[,]
        {
            { orgId1, "Navegadores & Companhia Lda.", "Rua das Âncoras, 123, Lisboa", "500100200", "Navegadores" }
        }
    );

    // PhysicalResources
    migrationBuilder.InsertData(
        table: "PhysicalResources",
        columns: new[] { "Id", "Description", "Type", "Capacity", "Status", "SetupTime" },
        values: new object[,]
        {
            { resourceIdGuindasteA, "Guindaste Post-Panamax 01", "Guindaste", 100.0, "Active", 30 },
            { Guid.NewGuid(), "Empilhadora 05", "Empilhadora", 5.0, "Active", 5 }
        }
    );

    // Qualifications
    migrationBuilder.InsertData(
        table: "Qualifications",
        columns: new[] { "Id", "Name" },
        values: new object[,]
        {
            { qualIdOperadorGuindaste, "Operador de Guindaste" },
            { qualIdCargaPerigosa, "Certificado de Carga Perigosa" }
        }
    );

    // StaffMembers
    migrationBuilder.InsertData(
        table: "StaffMembers",
        columns: new[] { "Id", "Name", "Email", "PhoneNumber", "Status", "OperationalWindow" },
        values: new object[,]
        {
            { staffIdJoao, "João Silva", "joao.silva@porto.com", 912345678, "Active", "08:00-17:00" }
        }
    );

    // StorageAreas
    migrationBuilder.InsertData(
        table: "StorageAreas",
        columns: new[] { "Id", "Code", "Designation", "StorageAreaType", "MaxCapacityTEUs", "CurrentOccupancyTEUs", "Active", "Location_Coordinates" },
        values: new object[,]
        {
            // StorageAreaType (int) 0=ContainerYard, 1=Refrigerated, 2=Hazardous, etc.
            { storageIdA1, "A1-REEFER", "Área de Contentores Refrigerados", 1, 500, 150, true, "41.147, -8.63" }
        }
    );

    // Users
    migrationBuilder.InsertData(
        table: "Users",
        columns: new[] { "Id", "Name", "Role", "Status" },
        values: new object[,]
        {
            // Role (int) 0=Admin, 1=Gestor, etc. | Status (int) 0=Active, 1=Inactive
            { userIdAdmin, "Administrador", 0, 0 }
        }
    );

    // VesselTypes
    migrationBuilder.InsertData(
        table: "VesselTypes",
        columns: new[] { "Id", "Name", "Description", "Capacity", "MaxRows", "MaxBays", "MaxTiers", "Active" },
        values: new object[,]
        {
            { vesselTypeIdPanamax, "Porta-Contentores Panamax", "Navio porta-contentores de tamanho Panamax.", 5000, 13, 17, 9, true }
        }
    );

    // Vessels (depende de VesselTypes)
    migrationBuilder.InsertData(
        table: "Vessels",
        columns: new[] { "Id", "ImoNumber", "Name", "VesselTypeId", "Owner", "Operator", "Active" },
        values: new object[,]
        {
            { vesselIdLisboaVoyager, "IMO9000001", "Lisboa Voyager", vesselTypeIdPanamax, "Global Ship Owner Inc.", "Porto Operators", true }
        }
    );

    // Representatives (depende de Organizations)
    migrationBuilder.InsertData(
        table: "Representatives",
        columns: new[] { "Id", "Name", "Nationality", "Email", "PhoneNumber", "OrganizationId", "Status", "role" },
        values: new object[,]
        {
            // role (int) 0=Main, 1=Substitute, etc.
            { repIdMaria, "Maria Costa", "Portuguesa", "maria.costa@navegadores.com", "960000111", orgId1, "Active", 0 }
        }
    );

    // PhysicalResourceQualifications (Tabela de Junção)
    migrationBuilder.InsertData(
        table: "PhysicalResourceQualifications",
        columns: new[] { "PhysicalResourceId", "QualificationId" },
        values: new object[,]
        {
            { resourceIdGuindasteA, qualIdOperadorGuindaste } // Guindaste A requer Operador de Guindaste
        }
    );

    // StaffMemberQualifications (Tabela de Junção)
    migrationBuilder.InsertData(
        table: "StaffMemberQualifications",
        columns: new[] { "StaffMemberId", "QualificationId" },
        values: new object[,]
        {
            { staffIdJoao, qualIdOperadorGuindaste }, // João Silva é Operador de Guindaste
            { staffIdJoao, qualIdCargaPerigosa }     // João Silva também tem Carga Perigosa
        }
    );

    // StorageDockAssignments (Tabela de Junção)
    migrationBuilder.InsertData(
        table: "StorageDockAssignments",
        columns: new[] { "DockId", "StorageAreaId", "DistanceMeters" },
        values: new object[,]
        {
            { dockIdA, storageIdA1, 250.5 } // Distância do Dock A para a Área A1
        }
    );

    // CrewMember (depende de Vessels)
    migrationBuilder.InsertData(
        table: "CrewMember",
        columns: new[] { "Id", "Name", "CitizenId", "Nationality", "VesselId" },
        values: new object[,]
        {
            { 1, "Capitão Rui Santos", "12345678Z9", "Portuguesa", vesselIdLisboaVoyager }
        }
    );

    // VesselVisitNotifications (depende de Vessels e Representatives)
    migrationBuilder.InsertData(
        table: "VesselVisitNotifications",
        columns: new[] { "Id", "VesselId", "Status", "RepresentativeId", "CreatedAt", "ArrivalTime", "DepartureTime", "UnloadTime", "LoadTime" },
        values: new object[,]
        {
            { visitId1, vesselIdLisboaVoyager, "Pending", repIdMaria, DateTime.UtcNow.AddDays(-1), DateTime.UtcNow.AddDays(7), DateTime.UtcNow.AddDays(9), 12, 8 }
        }
    );

    // DockVesselTypes (Tabela de Junção)
    migrationBuilder.InsertData(
        table: "DockVesselTypes",
        columns: new[] { "AllowedVesselTypesId", "DockId" },
        values: new object[,]
        {
            { vesselTypeIdPanamax, dockIdA }, // Dock A permite navios Panamax
            { vesselTypeIdPanamax, dockIdB }  // Dock B também permite
        }
    );

    // LoadingCargoManifests (depende de VesselVisitNotifications)
    migrationBuilder.InsertData(
        table: "LoadingCargoManifests",
        columns: new[] { "Id", "VesselVisitNotificationId" },
        values: new object[,]
        {
            { loadingManifestId, visitId1 }
        }
    );

    // UnloadingCargoManifests (depende de VesselVisitNotifications)
    migrationBuilder.InsertData(
        table: "UnloadingCargoManifests",
        columns: new[] { "Id", "VesselVisitNotificationId" },
        values: new object[,]
        {
            { unloadingManifestId, visitId1 }
        }
    );

    // LoadingManifestContainers (depende de LoadingCargoManifests)
    migrationBuilder.InsertData(
        table: "LoadingManifestContainers",
        columns: new[] { "Id", "PayloadWeight", "ContentsDescription", "CargoManifestId" },
        values: new object[,]
        {
            { Guid.NewGuid(), 22000.0, "Componentes Eletrónicos", loadingManifestId }
        }
    );

    // UnloadingManifestContainers (depende de UnloadingCargoManifests)
    migrationBuilder.InsertData(
        table: "UnloadingManifestContainers",
        columns: new[] { "Id", "PayloadWeight", "ContentsDescription", "CargoManifestId" },
        values: new object[,]
        {
            { Guid.NewGuid(), 18500.0, "Têxteis", unloadingManifestId }
        }
    );

            migrationBuilder.CreateIndex(
                name: "IX_CrewMember_VesselId",
                table: "CrewMember",
                column: "VesselId");

            migrationBuilder.CreateIndex(
                name: "IX_DockVesselTypes_DockId",
                table: "DockVesselTypes",
                column: "DockId");

            migrationBuilder.CreateIndex(
                name: "IX_LoadingCargoManifests_VesselVisitNotificationId",
                table: "LoadingCargoManifests",
                column: "VesselVisitNotificationId");

            migrationBuilder.CreateIndex(
                name: "IX_LoadingManifestContainers_CargoManifestId",
                table: "LoadingManifestContainers",
                column: "CargoManifestId");

            migrationBuilder.CreateIndex(
                name: "IX_PhysicalResourceQualifications_QualificationId",
                table: "PhysicalResourceQualifications",
                column: "QualificationId");

            migrationBuilder.CreateIndex(
                name: "IX_Representatives_Email",
                table: "Representatives",
                column: "Email",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Representatives_OrganizationId",
                table: "Representatives",
                column: "OrganizationId");

            migrationBuilder.CreateIndex(
                name: "IX_Representatives_PhoneNumber",
                table: "Representatives",
                column: "PhoneNumber",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_StaffMemberQualifications_QualificationId",
                table: "StaffMemberQualifications",
                column: "QualificationId");

            migrationBuilder.CreateIndex(
                name: "IX_StorageAreas_Code",
                table: "StorageAreas",
                column: "Code",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_UnloadingCargoManifests_VesselVisitNotificationId",
                table: "UnloadingCargoManifests",
                column: "VesselVisitNotificationId");

            migrationBuilder.CreateIndex(
                name: "IX_UnloadingManifestContainers_CargoManifestId",
                table: "UnloadingManifestContainers",
                column: "CargoManifestId");

            migrationBuilder.CreateIndex(
                name: "IX_UserActivations_Token",
                table: "UserActivations",
                column: "Token",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Vessels_Name",
                table: "Vessels",
                column: "Name");

            migrationBuilder.CreateIndex(
                name: "IX_Vessels_Operator",
                table: "Vessels",
                column: "Operator");

            migrationBuilder.CreateIndex(
                name: "IX_Vessels_Owner",
                table: "Vessels",
                column: "Owner");

            migrationBuilder.CreateIndex(
                name: "IX_VesselTypes_Name",
                table: "VesselTypes",
                column: "Name");

            migrationBuilder.CreateIndex(
                name: "IX_VesselVisitNotifications_Status",
                table: "VesselVisitNotifications",
                column: "Status");

            migrationBuilder.CreateIndex(
                name: "IX_VesselVisitNotifications_VesselId",
                table: "VesselVisitNotifications",
                column: "VesselId");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "CrewMember");

            migrationBuilder.DropTable(
                name: "DockVesselTypes");

            migrationBuilder.DropTable(
                name: "LoadingManifestContainers");

            migrationBuilder.DropTable(
                name: "PhysicalResourceQualifications");

            migrationBuilder.DropTable(
                name: "Representatives");

            migrationBuilder.DropTable(
                name: "StaffMemberQualifications");

            migrationBuilder.DropTable(
                name: "StorageDockAssignments");

            migrationBuilder.DropTable(
                name: "UnloadingManifestContainers");

            migrationBuilder.DropTable(
                name: "UserActivations");

            migrationBuilder.DropTable(
                name: "Users");

            migrationBuilder.DropTable(
                name: "Docks");

            migrationBuilder.DropTable(
                name: "VesselTypes");

            migrationBuilder.DropTable(
                name: "LoadingCargoManifests");

            migrationBuilder.DropTable(
                name: "PhysicalResources");

            migrationBuilder.DropTable(
                name: "Organizations");

            migrationBuilder.DropTable(
                name: "Qualifications");

            migrationBuilder.DropTable(
                name: "StaffMembers");

            migrationBuilder.DropTable(
                name: "StorageAreas");

            migrationBuilder.DropTable(
                name: "UnloadingCargoManifests");

            migrationBuilder.DropTable(
                name: "VesselVisitNotifications");

            migrationBuilder.DropTable(
                name: "Vessels");
        }
    }
}
