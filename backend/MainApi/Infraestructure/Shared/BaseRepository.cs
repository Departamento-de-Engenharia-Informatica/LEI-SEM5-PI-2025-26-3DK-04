using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Infrastructure.Shared
{
    public class BaseRepository<TEntity, TEntityId> : IRepository<TEntity, TEntityId>
        where TEntity : Entity<TEntityId>
        where TEntityId : EntityId
    {
        private protected readonly DbSet<TEntity> _objs;
        private protected readonly DbContext _context;

        public BaseRepository(DbSet<TEntity> objs, DbContext context)
        {
            _objs = objs ?? throw new ArgumentNullException(nameof(objs));
            _context = context ?? throw new ArgumentNullException(nameof(context));
        }

        public async Task<List<TEntity>> GetAllAsync()
        {
            return await _objs.ToListAsync();
        }

        public async Task<TEntity> GetByIdAsync(TEntityId id)
        {
            return await _objs.Where(x => id.Equals(x.Id)).FirstOrDefaultAsync();
        }

        public async Task<List<TEntity>> GetByIdsAsync(List<TEntityId> ids)
        {
            return await _objs.Where(x => ids.Contains(x.Id)).ToListAsync();
        }

        public async Task<TEntity> AddAsync(TEntity obj)
        {
            var ret = await _objs.AddAsync(obj);
            await _context.SaveChangesAsync();
            return ret.Entity;
        }

        public async Task UpdateAsync(TEntity obj)
        {
            _objs.Update(obj);
            await _context.SaveChangesAsync();
        }

        public void Remove(TEntity obj)
        {
            _objs.Remove(obj);
        }
    }
}