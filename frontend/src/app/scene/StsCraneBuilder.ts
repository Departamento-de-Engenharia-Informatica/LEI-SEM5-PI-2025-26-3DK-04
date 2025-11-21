import * as THREE from 'three';

export enum CraneType {
  STS = "ShipToShore",
}

export class StsCraneBuilder {
  // Helper: cria um cilindro entre dois pontos (usa para diagonais e travessas inclinadas)
  private static createCylinderBetweenPoints(
    start: THREE.Vector3,
    end: THREE.Vector3,
    radius: number,
    radialSegments: number,
    material: THREE.Material
  ): THREE.Mesh {
    const dir = new THREE.Vector3().subVectors(end, start);
    const length = dir.length();
    const geom = new THREE.CylinderGeometry(radius, radius, length, radialSegments);
    const mesh = new THREE.Mesh(geom, material);
    const mid = new THREE.Vector3().addVectors(start, end).multiplyScalar(0.5);
    mesh.position.copy(mid);
    const up = new THREE.Vector3(0, 1, 0);
    const quat = new THREE.Quaternion().setFromUnitVectors(up, dir.clone().normalize());
    mesh.quaternion.copy(quat);
    return mesh;
  }

  /**
   * createCrane
   * @param width - base width (affeta dx/dz e espessuras)
   * @param height - altura das pernas
   * @param boomLength - comprimento da lança
   * @param position - posição final no mundo (y = 0 -> chão)
   * @param id - id para naming
   * @param type - CraneType (mantido para compatibilidade)
   * @param options.debug - se true, adiciona AxesHelper e BoxHelper para debug visual
   */
  static createCrane(
    width: number,
    height: number,
    boomLength: number,
    position: THREE.Vector3,
    id: number,
    type: CraneType = CraneType.STS,
    options: { debug?: boolean } = {}
  ): THREE.Group {
    const { debug = false } = options;

    const crane = new THREE.Group();
    crane.name = `STS_Crane_${id}`;

    // ---------------------------
    // Materiais
    // ---------------------------
    const pillarMaterial = new THREE.MeshStandardMaterial({ color: 0xe6b800, metalness: 0.35, roughness: 0.55 });
    const boomMaterial = new THREE.MeshStandardMaterial({ color: 0xffa500, metalness: 0.4, roughness: 0.35 });
    const railMaterial = new THREE.MeshStandardMaterial({ color: 0x404040, metalness: 0.6, roughness: 0.5 });
    const trolleyMaterial = new THREE.MeshStandardMaterial({ color: 0xffffff, metalness: 0.25, roughness: 0.6 });
    const cabinMaterial = new THREE.MeshStandardMaterial({ color: 0x1f4e79, metalness: 0.15, roughness: 0.6 });
    const counterMaterial = new THREE.MeshStandardMaterial({ color: 0x333333, metalness: 0.3, roughness: 0.5 });
    const lineMaterial = new THREE.LineBasicMaterial({ color: 0x000000 });

    // Parâmetros derivados
    const pillarRadius = Math.max(width * 0.035, 0.05);
    const dx = width * 0.35;
    const dz = dx; // mesma medida → torre quadrada

    const baseThickness = Math.max(height * 0.04, 0.05);

    // ---------------------------
    // BASE / PLATAFORMA / RAILS
    // ---------------------------
    const baseGroup = new THREE.Group();
    baseGroup.name = 'baseGroup';
    // base com exatamente a mesma largura/ comprimento das pernas
    const baseGeom = new THREE.BoxGeometry(dx * 2, baseThickness, dz * 2);
    const baseMesh = new THREE.Mesh(baseGeom, railMaterial);
    baseMesh.position.set(0, baseThickness / 2, 0);
    baseGroup.add(baseMesh);

    // pés sob cada perna
    // pés sob cada perna (pequenos pads)
    const footGeom = new THREE.BoxGeometry(pillarRadius * 1.6, baseThickness / 2, pillarRadius * 1.6);
    const footPositions = [
      [-dx, 0, -dz],
      [-dx, 0, dz],
      [dx, 0, -dz],
      [dx, 0, dz]
    ];
    footPositions.forEach(p => {
      const foot = new THREE.Mesh(footGeom, railMaterial);
      foot.position.set(p[0], baseThickness / 4, p[2]);
      baseGroup.add(foot);
    });

    crane.add(baseGroup);

    // ---------------------------
    // TORRE (4 pernas + travessas + diagonais)
    // ---------------------------
    const towerGroup = new THREE.Group();
    towerGroup.name = 'tower';
    const pillarPositions = [
      new THREE.Vector3(-dx, 0, -dz),
      new THREE.Vector3(-dx, 0, dz),
      new THREE.Vector3(dx, 0, -dz),
      new THREE.Vector3(dx, 0, dz),
    ];
    const pillars: THREE.Mesh[] = [];
    pillarPositions.forEach(pos => {
      const geom = new THREE.CylinderGeometry(pillarRadius, pillarRadius, height, 12);
      const pillar = new THREE.Mesh(geom, pillarMaterial);
      pillar.position.set(pos.x, height / 2 + baseThickness, pos.z);
      towerGroup.add(pillar);
      pillars.push(pillar);
    });
    // --- top ring (liga as 4 pernas no topo) ---
    const topY = baseThickness + height; // topo das pernas
    const topRingRadius = pillarRadius * 0.9;


    // espessura dos perfis (consistente entre torre e boom)
    const towerBeamRadius = pillarRadius * 0.7;   // travessas da torre (ligeiramente menores que o pilar)
    const beamRadius = Math.max(width * 0.25 * 0.02, 0.02); // truss beams (boom) — garantido mínimo

// ligações horizontais no topo (rectangular ring)
    const topBeam1 = StsCraneBuilder.createCylinderBetweenPoints(
      new THREE.Vector3(-dx, topY, -dz),
      new THREE.Vector3(dx, topY, -dz),
      towerBeamRadius,
      8,
      pillarMaterial
    );
    const topBeam2 = StsCraneBuilder.createCylinderBetweenPoints(
      new THREE.Vector3(-dx, topY, dz),
      new THREE.Vector3(dx, topY, dz),
      towerBeamRadius,
      8,
      pillarMaterial
    );
    const topBeam3 = StsCraneBuilder.createCylinderBetweenPoints(
      new THREE.Vector3(-dx, topY, -dz),
      new THREE.Vector3(-dx, topY, dz),
      towerBeamRadius,
      8,
      pillarMaterial
    );
    const topBeam4 = StsCraneBuilder.createCylinderBetweenPoints(
      new THREE.Vector3(dx, topY, -dz),
      new THREE.Vector3(dx, topY, dz),
      towerBeamRadius,
      8,
      pillarMaterial
    );
    towerGroup.add(topBeam1, topBeam2, topBeam3, topBeam4);

// pequena placa de topo (cap plate) em cada pilar para soldadura visual
    pillars.forEach(p => {
      const cap = new THREE.Mesh(
        new THREE.BoxGeometry(pillarRadius * 1.2, pillarRadius * 0.3, pillarRadius * 1.2),
        railMaterial
      );
      cap.position.set(p.position.x, topY + (pillarRadius * 0.15), p.position.z);
      towerGroup.add(cap);
    });

    // travessas horizontais e diagonais em níveis
    const nLevels = Math.max(3, Math.floor(height / 6));
    for (let i = 1; i <= nLevels; i++) {
      const y = baseThickness + (height / (nLevels + 1)) * i;
      const beamXZ = StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(-dx, y, -dz),
        new THREE.Vector3(dx, y, -dz),
        towerBeamRadius,
        8,
        pillarMaterial
      );
      const beamXZ2 = StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(-dx, y, dz),
        new THREE.Vector3(dx, y, dz),
        towerBeamRadius,
        8,
        pillarMaterial
      );
      const beamZZ = StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(-dx, y, -dz),
        new THREE.Vector3(-dx, y, dz),
        towerBeamRadius,
        8,
        pillarMaterial
      );
      const beamZZ2 = StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(dx, y, -dz),
        new THREE.Vector3(dx, y, dz),
        towerBeamRadius,
        8,
        pillarMaterial
      );
      towerGroup.add(beamXZ, beamXZ2, beamZZ, beamZZ2);

      // diagonais simples (X) nas faces
      const offset = (height / (nLevels + 1)) * 0.45;
      towerGroup.add(StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(-dx, y - offset, -dz),
        new THREE.Vector3(-dx, y + offset, dz),
        pillarRadius * 0.7, 6, pillarMaterial
      ));
      towerGroup.add(StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(dx, y - offset, -dz),
        new THREE.Vector3(dx, y + offset, dz),
        pillarRadius * 0.7, 6, pillarMaterial
      ));
      towerGroup.add(StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(-dx, y - offset, -dz),
        new THREE.Vector3(dx, y + offset, -dz),
        pillarRadius * 0.7, 6, pillarMaterial
      ));
      towerGroup.add(StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(-dx, y + offset, dz),
        new THREE.Vector3(dx, y - offset, dz),
        pillarRadius * 0.7, 6, pillarMaterial
      ));
    }

    crane.add(towerGroup);

// -----------------------------------------
// BOOM 3D STRUCTURE (TRIANGULAR TRUSS)
// -----------------------------------------

    const boomBaseWidth = width * 0.60;          // base do triângulo = largura total da base
    const boomHeightTri = height * 0.15;  // altura do triângulo (ajusta se quiseres)

    const boomGroup = new THREE.Group();
    boomGroup.name = "boomGroup";

// posição do boom conectada à torre
    // alinhar boom com hinge para ligação perfeita

// Material uniforme amarelo industrial
    const trussMaterial = new THREE.MeshStandardMaterial({
      color: 0xffcc00,
      metalness: 0.4,
      roughness: 0.55
    });

// Função auxiliar
    function beam(x1: number, y1: number, z1: number, x2: number, y2: number, z2: number) {
      return StsCraneBuilder.createCylinderBetweenPoints(
        new THREE.Vector3(x1, y1, z1),
        new THREE.Vector3(x2, y2, z2),
        boomBaseWidth * 0.035,
        10,
        trussMaterial
      );
    }

// --------------------------------------------------
// VÉRTICES DO TRIÂNGULO (secção transversal)
// --------------------------------------------------
//
//   Top (0, +H, 0)
//   Left (-W/2, 0, 0)
//   Right(+W/2, 0, 0)
//
// --------------------------------------------------

    const halfBase = boomBaseWidth / 2;
    const triH = boomHeightTri;

    const start = 0;
    const end = boomLength;

// Pontos iniciais
    const A1 = { x: start, y: triH, z: 0 };        // topo
    const B1 = { x: start, y: 0,     z: -halfBase }; // base esquerda
    const C1 = { x: start, y: 0,     z: +halfBase }; // base direita

// Pontos finais
    const A2 = { x: end, y: triH, z: 0 };
    const B2 = { x: end, y: 0,   z: -halfBase };
    const C2 = { x: end, y: 0,   z: +halfBase };

// -----------------------------------------
// VIGAS LONGITUDINAIS DO TRIÂNGULO
// -----------------------------------------

    boomGroup.add(beam(A1.x, A1.y, A1.z, A2.x, A2.y, A2.z)); // topo
    boomGroup.add(beam(B1.x, B1.y, B1.z, B2.x, B2.y, B2.z)); // base esquerda
    boomGroup.add(beam(C1.x, C1.y, C1.z, C2.x, C2.y, C2.z)); // base direita

// -----------------------------------------
// TRAVESSAS VERTICAIS E DIAGONAIS DO TRIÂNGULO
// -----------------------------------------

    const segments = 6;

    for (let i = 1; i < segments; i++) {
      const x = (boomLength / segments) * i;

      const Ai = { x, y: triH, z: 0 };
      const Bi = { x, y: 0, z: -halfBase };
      const Ci = { x, y: 0, z: +halfBase };

      // Travessa centro-base
      boomGroup.add(beam(Ai.x, Ai.y, Ai.z, Bi.x, Bi.y, Bi.z));
      boomGroup.add(beam(Ai.x, Ai.y, Ai.z, Ci.x, Ci.y, Ci.z));

      // Travessa base-base
      boomGroup.add(beam(Bi.x, Bi.y, Bi.z, Ci.x, Ci.y, Ci.z));
    }

// -----------------------------------------
// DIAGONAIS EM X AO LONGO DE TODA A TRELIÇA
// -----------------------------------------

    for (let i = 0; i < segments; i++) {
      const x1 = (boomLength / segments) * i;
      const x2 = (boomLength / segments) * (i + 1);

      // topo ↔ base esquerda
      boomGroup.add(beam(x1, triH, 0, x2, 0, -halfBase));
      boomGroup.add(beam(x1, 0, -halfBase, x2, triH, 0));

      // topo ↔ base direita
      boomGroup.add(beam(x1, triH, 0, x2, 0, halfBase));
      boomGroup.add(beam(x1, 0, halfBase, x2, triH, 0));

      // base esquerda ↔ base direita (cruz inferior)
      boomGroup.add(beam(x1, 0, -halfBase, x2, 0, halfBase));
      boomGroup.add(beam(x1, 0, halfBase, x2, 0, -halfBase));
    }



    // Ligação entre torre/base e boom (hinge block) — posicionado relativo a dx
    const hingeWidth = width * 0.18;
    const hingeHeight = height * 0.12;
    const hingeDepth = width * 0.12;

    const hingeBlock = new THREE.Mesh(
      new THREE.BoxGeometry(hingeWidth, hingeHeight, hingeDepth),
      pillarMaterial
    );

// posiciona o hinge logo atrás do topo da torre (ligeiro recuo)
    const hingeX = -dx - hingeWidth * 0.45; // ligeiramente atrás da face posterior das pernas
    const hingeY = baseThickness + height * 0.88;
    hingeBlock.position.set(hingeX, hingeY, 0);

    crane.add(hingeBlock);

    // ligações curtas/top gussets da top ring até ao hinge
    const gussetOffsetY = topY - (height * 0.06);
    const gussetPoints = [
      new THREE.Vector3(-dx * 0.6, gussetOffsetY, -dz * 0.6),
      new THREE.Vector3(dx * 0.6, gussetOffsetY, -dz * 0.6),
      new THREE.Vector3(-dx * 0.6, gussetOffsetY, dz * 0.6),
      new THREE.Vector3(dx * 0.6, gussetOffsetY, dz * 0.6)
    ];

    gussetPoints.forEach(pt => {
      // pequena barra até ao hingeBlock
      const conn = StsCraneBuilder.createCylinderBetweenPoints(
        pt,
        new THREE.Vector3(hingeBlock.position.x + hingeWidth * 0.18, hingeBlock.position.y + hingeHeight * 0.05, 0),
        towerBeamRadius * 0.9,
        8,
        pillarMaterial
      );
      crane.add(conn);

      // pequena placa na união (visual)
      const plate = new THREE.Mesh(
        new THREE.BoxGeometry(towerBeamRadius * 1.6, towerBeamRadius * 0.4, towerBeamRadius * 1.6),
        railMaterial
      );
      plate.position.copy(pt.clone().lerp(new THREE.Vector3(hingeBlock.position.x + hingeWidth * 0.18, hingeBlock.position.y + hingeHeight * 0.05, 0), 0.25));
      crane.add(plate);
    });

    boomGroup.position.set(hingeBlock.position.x - hingeWidth * 0.5 + beamRadius * 0.15, hingeBlock.position.y + hingeHeight * 0.05, -5);
    // boom já posicionada no X e Z
    boomGroup.position.set(
      hingeBlock.position.x + hingeWidth * 0.5 + beamRadius * 0.15,
      hingeBlock.position.y - hingeHeight * 0.05,
      0
    );

// ajuste de altura total da boom
    const boomClearance = boomHeightTri * 0.25;
    const boomOffsetDown = boomHeightTri * 0.75; // quanto queres descer

    boomGroup.position.y =
      baseThickness + height + boomHeightTri / 2 + boomClearance
      - boomOffsetDown;

    const tipHeight = boomHeightTri;  // altura do fecho
    const tipBaseWidth = halfBase;    // largura da base do fecho

    function closeBoomTip(boomGroup: THREE.Group, x: number, triH: number, halfBase: number, isStart: boolean) {
      const sign = isStart ? -1 : 1;

      const top = { x, y: triH, z: 0 };
      const left = { x, y: 0, z: -halfBase };
      const right = { x, y: 0, z: halfBase };

      // Vigas inclinadas topo → base esquerda/direita (formam /|\)
      boomGroup.add(beam(top.x, top.y, top.z, left.x, left.y, left.z));
      boomGroup.add(beam(top.x, top.y, top.z, right.x, right.y, right.z));

      // Viga base esquerda ↔ direita (fecha a base do triângulo)
      boomGroup.add(beam(left.x, left.y, left.z, right.x, right.y, right.z));
    }

    // Início da boom
    // Ponta inicial da boom
    closeBoomTip(boomGroup, 0, triH, halfBase, true);

// Ponta final da boom
    closeBoomTip(boomGroup, boomLength, triH, halfBase, false);



    crane.add(boomGroup);


    // ---------------------------
// TROLLEY (adaptação à boom triangular)
// ---------------------------
    const trolleyGroup = new THREE.Group();
    trolleyGroup.name = "trolley";

// largura da base do triângulo da boom
    const triBase = width * 0.25; // mesma largura da base do triângulo que definiste
    const triHeight = height * 0.18; // altura da boom triangular

// dimensões do corpo do trolley (proporcional ao vão inferior do triângulo)
    const trolleyBodyWidth = triBase * 0.6;
    const trolleyBodyHeight = triHeight * 0.4;
    const trolleyBodyDepth = triBase * 0.3;

    const trolleyBody = new THREE.Mesh(
      new THREE.BoxGeometry(trolleyBodyWidth, trolleyBodyHeight, trolleyBodyDepth),
      trolleyMaterial
    );
    trolleyBody.castShadow = true;
    trolleyBody.receiveShadow = true;

// pendurado logo abaixo da parte inferior da treliça (vértice inferior do triângulo)
    trolleyBody.position.set(0, -trolleyBodyHeight * 0.6, 0);
    trolleyGroup.add(trolleyBody);

// ---------------------------
// RODAS SUPERIORES (assentam no topo do corpo do trolley)
// ---------------------------
    const wheelRadius = triBase * 0.06;
    const wheelThickness = trolleyBodyDepth * 0.8;

    const wheelGeom = new THREE.CylinderGeometry(wheelRadius, wheelRadius, wheelThickness, 12);

    const createWheel = (z: number) => {
      const w = new THREE.Mesh(wheelGeom, trolleyMaterial);
      w.rotation.z = Math.PI / 2;
      w.position.set(0, trolleyBodyHeight * 0.5, z);
      trolleyGroup.add(w);
    };

// rodas nas extremidades da base do trolley
    createWheel(trolleyBodyDepth * 0.5);
    createWheel(-trolleyBodyDepth * 0.5);

// ---------------------------
// GANCHO
// ---------------------------
    const hookHeight = height * 0.58;

    const hookGeom = new THREE.BufferGeometry().setFromPoints([
      new THREE.Vector3(0, -trolleyBodyHeight * 0.5, 0),
      new THREE.Vector3(0, -trolleyBodyHeight * 0.5 - hookHeight * 0.5, 0),
      new THREE.Vector3(0, -trolleyBodyHeight * 0.5 - hookHeight, 0)
    ]);

    const hookLine = new THREE.Line(hookGeom, lineMaterial);
    trolleyGroup.add(hookLine);

    const hookBlock = new THREE.Mesh(
      new THREE.BoxGeometry(triBase * 0.12, triBase * 0.12, triBase * 0.12),
      trolleyMaterial
    );
    hookBlock.position.set(0, -trolleyBodyHeight * 0.5 - hookHeight, 0);
    trolleyGroup.add(hookBlock);

// ---------------------------
// POSIÇÃO AO LONGO DO BOOM
// ---------------------------
    const trolleyX = boomLength * 0.45;
    trolleyGroup.position.set(trolleyX, 0, 0);

    boomGroup.add(trolleyGroup);





    // ---------------------------
    // CABINE (ligada lateralmente à torre)
    // ---------------------------
    const cabinWidth = width * 0.22;
    const cabinHeight = height * 0.18;
    const cabinDepth = width * 0.12;
    const cabin = new THREE.Mesh(new THREE.BoxGeometry(cabinWidth, cabinHeight, cabinDepth), cabinMaterial);
    cabin.name = 'cabin';
    cabin.position.set(
      dx * 0.55,                              // deslocada para o lado do boom
      baseThickness + height * 0.75,          // perto do topo da torre
      -dz - cabinDepth * 0.8                  // para a frente da torre
    );

    crane.add(cabin);

    // ---------------------------
    // CONTRAPESO (atrás da torre)
    // ---------------------------
    const counterGeom = new THREE.BoxGeometry(width * 0.5, height * 0.22, width * 0.28);
    const counter = new THREE.Mesh(counterGeom, counterMaterial);
    counter.name = 'counter';
    counter.position.set(-Math.max(width * 0.7, dx * 1.3), baseThickness + height * 0.9, 0);
    crane.add(counter);

    // ---------------------------
    // CABOS / LINHAS ADICIONAIS (visual)
    // ---------------------------
    /*
    const boomTipLocal = new THREE.Vector3(boomLength * Math.cos(-Math.PI / 12), -Math.abs(Math.sin(-Math.PI / 12)) * boomLength, 0);
    const cablePoints = [boomTipLocal, new THREE.Vector3((boomTipLocal.x + trolleyX) / 2, (boomTipLocal.y + 0) / 2 + 0.2, 0), new THREE.Vector3(trolleyX, 0, 0)];
    const cableGeo = new THREE.BufferGeometry().setFromPoints(cablePoints);
    const cableLine = new THREE.Line(cableGeo, lineMaterial);
    boomGroup.add(cableLine);
    */
    // -------------------------------------------
// Tower Peak (pico da torre + conexões por cabos)
// -------------------------------------------
    const peakHeight = height * 0.25;

    const towerPeak = new THREE.Mesh(
      new THREE.CylinderGeometry(pillarRadius * 0.7, pillarRadius * 0.7, peakHeight, 12),
      pillarMaterial
    );

    towerPeak.position.set(0, baseThickness + height + peakHeight * 0.5, 0);
    towerPeak.name = "towerPeak";
    crane.add(towerPeak);

// ponto superior
    const peakTop = new THREE.Vector3(0, towerPeak.position.y + peakHeight * 0.5, 0);

// cabo para contrapeso
    {
      const geo = new THREE.BufferGeometry().setFromPoints([
        peakTop,
        new THREE.Vector3(counter.position.x, counter.position.y + height * 0.05, 0)
      ]);
      crane.add(new THREE.Line(geo, lineMaterial));
    }

// cabo frontal para ponta da boom
    {
      const boomTipWorld = new THREE.Vector3();
      boomGroup.localToWorld(boomTipWorld.set(boomLength, 0, 0));

      const geo = new THREE.BufferGeometry().setFromPoints([
        peakTop,
        boomTipWorld
      ]);
      crane.add(new THREE.Line(geo, lineMaterial));
    }


    // ---------------------------
    // Debug helpers (opcional)
    // ---------------------------
    if (debug) {
      crane.add(new THREE.AxesHelper(Math.max(2, width)));
      const box = new THREE.BoxHelper(crane, 0x00ff00);
      crane.add(box);

      // helpers locais
      towerGroup.add(new THREE.AxesHelper(Math.max(1.5, width * 0.5)));
      boomGroup.add(new THREE.AxesHelper(Math.max(1.5, width * 0.5)));
      trolleyGroup.add(new THREE.AxesHelper(Math.max(1.0, width * 0.25)));
    }

    // ---------------------------
    // POSIÇÃO FINAL
    // ---------------------------
    crane.position.copy(position);

    return crane;
  }
}
