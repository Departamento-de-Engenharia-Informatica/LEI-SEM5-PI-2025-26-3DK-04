import * as THREE from 'three';

export enum CraneType {
  STS = "ShipToShore",
}

export class StsCraneBuilder {

  static createCrane(
    width: number,
    height: number,
    boomLength: number,
    position: THREE.Vector3,
    id: number,
    type: CraneType = CraneType.STS
  ): THREE.Group {

    const crane = new THREE.Group();
    crane.name = `STS_Crane_${id}`;

    // ---------------------------
    // BASE / RAILS
    // ---------------------------
    const railGeometry = new THREE.BoxGeometry(width, height * 0.05, width * 1.2);
    const railMaterial = new THREE.MeshStandardMaterial({ color: 0x404040, metalness: 0.5, roughness: 0.6 });

    const rail = new THREE.Mesh(railGeometry, railMaterial);
    rail.position.set(0, height * 0.025, 0);
    crane.add(rail);


    // ---------------------------
    // TORRE (estrutura metálica em cruz)
    // ---------------------------
    const towerGroup = new THREE.Group();

    const pillarGeometry = new THREE.BoxGeometry(width * 0.15, height, width * 0.15);
    const pillarMaterial = new THREE.MeshStandardMaterial({ color: 0xe6b800, metalness: 0.4 });

    const leftPillar = new THREE.Mesh(pillarGeometry, pillarMaterial);
    leftPillar.position.set(-width * 0.3, height / 2, 0);

    const rightPillar = new THREE.Mesh(pillarGeometry, pillarMaterial);
    rightPillar.position.set(width * 0.3, height / 2, 0);

    towerGroup.add(leftPillar);
    towerGroup.add(rightPillar);

    // Travessas horizontais
    const beamGeometry = new THREE.BoxGeometry(width * 0.7, height * 0.06, width * 0.08);
    const beam = new THREE.Mesh(beamGeometry, pillarMaterial);

    beam.position.set(0, height * 0.65, 0);
    towerGroup.add(beam);

    crane.add(towerGroup);


    // ---------------------------
    // LANÇA / BOOM
    // ---------------------------
    const boomGroup = new THREE.Group();

    const boomGeometry = new THREE.BoxGeometry(boomLength, height * 0.07, width * 0.15);
    const boomMaterial = new THREE.MeshStandardMaterial({ color: 0xffa500, metalness: 0.4 });

    const boom = new THREE.Mesh(boomGeometry, boomMaterial);
    boom.position.set(boomLength / 2, 0, 0);

    boomGroup.add(boom);

    // Suporte vertical da lança
    const boomSupportGeometry = new THREE.BoxGeometry(width * 0.15, height * 0.3, width * 0.15);
    const boomSupport = new THREE.Mesh(boomSupportGeometry, pillarMaterial);
    boomSupport.position.set(0, -height * 0.15, 0);

    boomGroup.add(boomSupport);

    boomGroup.position.set(0, height * 0.75, 0);
    boomGroup.rotation.z = -Math.PI / 12;

    crane.add(boomGroup);


    // ---------------------------
    // TROLLEY (carrinho)
    // ---------------------------
    const trolleyGeometry = new THREE.BoxGeometry(width * 0.25, height * 0.08, width * 0.2);
    const trolleyMaterial = new THREE.MeshStandardMaterial({ color: 0xffffff, metalness: 0.3 });

    const trolley = new THREE.Mesh(trolleyGeometry, trolleyMaterial);
    trolley.position.set(boomLength * 0.4, height * 0.75, width * 0);

    crane.add(trolley);


    // ---------------------------
    // CABINE DO OPERADOR
    // ---------------------------
    const cabinGeometry = new THREE.BoxGeometry(width * 0.25, height * 0.2, width * 0.15);
    const cabinMaterial = new THREE.MeshStandardMaterial({ color: 0x1f4e79, metalness: 0.2 });

    const cabin = new THREE.Mesh(cabinGeometry, cabinMaterial);
    cabin.position.set(width * 0.4, height * 0.6, width * 0.2);

    crane.add(cabin);


    // ---------------------------
    // CABOS (linhas visuais simples)
    // ---------------------------
    const cableMaterial = new THREE.LineBasicMaterial({ color: 0x000000 });

    const points = [];
    points.push(new THREE.Vector3(boomLength * 0.4, height * 0.75, 0));
    points.push(new THREE.Vector3(boomLength * 0.4, height * 0.2, 0));

    const cableGeometry = new THREE.BufferGeometry().setFromPoints(points);
    const cable = new THREE.Line(cableGeometry, cableMaterial);

    crane.add(cable);


    // ---------------------------
    // CONTRAPESO
    // ---------------------------
    const counterGeometry = new THREE.BoxGeometry(width * 0.4, height * 0.3, width * 0.3);
    const counterMaterial = new THREE.MeshStandardMaterial({ color: 0x333333, metalness: 0.3 });

    const counterWeight = new THREE.Mesh(counterGeometry, counterMaterial);
    counterWeight.position.set(-width * 0.6, height * 0.75, 0);

    crane.add(counterWeight);


    // ---------------------------
    // POSIÇÃO FINAL
    // ---------------------------
    crane.position.copy(position);

    return crane;
  }
}
