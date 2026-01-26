using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;
using OraculoCorporativo.Models;

namespace OraculoCorporativo.Data
{
    public class AppDbContext : DbContext
    {
        public AppDbContext(DbContextOptions<AppDbContext> options) : base(options) { }

        public DbSet<SystemInfo> SystemInfos { get; set; }
    }
}