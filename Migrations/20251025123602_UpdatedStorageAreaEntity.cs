using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace DDDNetCore.Migrations
{
    /// <inheritdoc />
    public partial class UpdatedStorageAreaEntity : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "StorageDockAssignment");

            migrationBuilder.RenameColumn(
                name: "Type",
                table: "StorageAreas",
                newName: "Code");

            migrationBuilder.RenameColumn(
                name: "Location",
                table: "StorageAreas",
                newName: "Designation");

            migrationBuilder.AddColumn<bool>(
                name: "Active",
                table: "StorageAreas",
                type: "boolean",
                nullable: false,
                defaultValue: false);

            migrationBuilder.AddColumn<string>(
                name: "Location_Coordinates",
                table: "StorageAreas",
                type: "text",
                nullable: true);

            migrationBuilder.AddColumn<string>(
                name: "Location_Description",
                table: "StorageAreas",
                type: "text",
                nullable: true);

            migrationBuilder.AddColumn<int>(
                name: "StorageAreaType",
                table: "StorageAreas",
                type: "integer",
                nullable: false,
                defaultValue: 0);

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

            migrationBuilder.CreateIndex(
                name: "IX_StorageAreas_Code",
                table: "StorageAreas",
                column: "Code",
                unique: true);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "StorageDockAssignments");

            migrationBuilder.DropIndex(
                name: "IX_StorageAreas_Code",
                table: "StorageAreas");

            migrationBuilder.DropColumn(
                name: "Active",
                table: "StorageAreas");

            migrationBuilder.DropColumn(
                name: "Location_Coordinates",
                table: "StorageAreas");

            migrationBuilder.DropColumn(
                name: "Location_Description",
                table: "StorageAreas");

            migrationBuilder.DropColumn(
                name: "StorageAreaType",
                table: "StorageAreas");

            migrationBuilder.RenameColumn(
                name: "Designation",
                table: "StorageAreas",
                newName: "Location");

            migrationBuilder.RenameColumn(
                name: "Code",
                table: "StorageAreas",
                newName: "Type");

            migrationBuilder.CreateTable(
                name: "StorageDockAssignment",
                columns: table => new
                {
                    StorageAreaId = table.Column<Guid>(type: "uuid", nullable: false),
                    DockId = table.Column<Guid>(type: "uuid", nullable: false),
                    DistanceMeters = table.Column<double>(type: "double precision", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StorageDockAssignment", x => new { x.StorageAreaId, x.DockId });
                    table.ForeignKey(
                        name: "FK_StorageDockAssignment_StorageAreas_StorageAreaId",
                        column: x => x.StorageAreaId,
                        principalTable: "StorageAreas",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });
        }
    }
}
