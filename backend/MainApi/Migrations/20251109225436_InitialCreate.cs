using System;
using Microsoft.EntityFrameworkCore.Migrations;
using Npgsql.EntityFrameworkCore.PostgreSQL.Metadata;

#nullable disable

namespace DDDNetCore.Migrations
{
    /// <inheritdoc />
    public partial class InitialCreate : Migration
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
                    StorageAreaType = table.Column<int>(type: "integer", nullable: false),
                    Location_Coordinates = table.Column<string>(type: "text", nullable: true),
                    Location_Description = table.Column<string>(type: "text", nullable: true),
                    Code = table.Column<string>(type: "character varying(50)", maxLength: 50, nullable: false),
                    Designation = table.Column<string>(type: "character varying(100)", maxLength: 100, nullable: false),
                    MaxCapacityTEUs = table.Column<int>(type: "integer", nullable: false),
                    CurrentOccupancyTEUs = table.Column<int>(type: "integer", nullable: false),
                    Active = table.Column<bool>(type: "boolean", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StorageAreas", x => x.Id);
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
                    Status = table.Column<string>(type: "text", nullable: false)
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
                    CreatedAt = table.Column<DateTime>(type: "timestamp with time zone", nullable: false)
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
