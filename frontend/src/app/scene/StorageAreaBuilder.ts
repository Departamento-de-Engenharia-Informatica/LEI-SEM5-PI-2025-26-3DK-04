import * as THREE from 'three';

export enum StorageAreaType {
  Yard = 0,
  Warehouse = 1,
  Refrigerated = 2,
  Other = 3
}

export class StorageAreaBuilder {

  static createStorageArea(
    width: number,
    depth: number,
    position: THREE.Vector3,
    id: number,
    type: StorageAreaType
  ): THREE.Mesh {

    let geometry: THREE.BoxGeometry;
    let material: THREE.MeshStandardMaterial;

    switch (type) {

      case StorageAreaType.Yard:
        geometry = new THREE.BoxGeometry(width, 0.4, depth);
        material = new THREE.MeshStandardMaterial({
          color: 0xC2B280, // cor de terra / areia
          roughness: 1,
          metalness: 0
        });
        break;

      case StorageAreaType.Warehouse:
        geometry = new THREE.BoxGeometry(width, 6, depth);
        material = new THREE.MeshStandardMaterial({
          color: 0x62686d, // cinza industrial
          roughness: 0.7,
          metalness: 0.2
        });
        break;

      case StorageAreaType.Refrigerated:
        geometry = new THREE.BoxGeometry(width, 2.5, depth);
        material = new THREE.MeshStandardMaterial({
          color: 0xa8def0, // azul claro
          roughness: 0.4,
          metalness: 0.1
        });
        break;

      case StorageAreaType.Other:
      default:
        geometry = new THREE.BoxGeometry(width, 0.5, depth);
        material = new THREE.MeshStandardMaterial({
          color: 0x9fbf8f, // verde seco
          roughness: 0.8,
          metalness: 0
        });
        break;
    }

    const area = new THREE.Mesh(geometry, material);
    area.position.copy(position);
    area.name = `Storage_${id}`;
    area.userData = { storageId: id, type };

    return area;
  }
}
