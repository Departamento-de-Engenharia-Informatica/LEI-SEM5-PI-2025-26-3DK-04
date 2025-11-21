describe('Manage Docks Page', () => {
  const userStub = {
    name: 'Test Admin',
    email: 'admin@test.com',
    role: 'admin',
    roles: ['admin'],
  };

  const initialDocks = [
    {
      id: '1',
      name: 'Dock Alpha',
      location: 'North Zone',
      length: 300,
      depth: 14,
      maxDraft: 10,
    },
    {
      id: '2',
      name: 'Dock Beta',
      location: 'South Zone',
      length: 250,
      depth: 11,
      maxDraft: 9,
    },
  ];

  const vesselTypesMock = [
    { id: '1', name: 'Cargo' },
    { id: '2', name: 'Tanker' },
  ];

  let docksStub: any[];

  beforeEach(() => {
    // Reset state
    docksStub = [...initialDocks];

    cy.window().then(win => {
      win.sessionStorage.clear();
      win.localStorage.clear();
    });

    // Fake login mínimo (simula token guardado)
    cy.window().then(win => {
      win.localStorage.setItem('access_token', 'fake-token');
    });

    // Mock auth
    cy.intercept('GET', '**/authtest/me', {
      statusCode: 200,
      body: userStub,
    }).as('authMe');

    // GET /api/Dock
    cy.intercept('GET', '**/api/Dock*', (req) => {
      req.reply({
        statusCode: 200,
        body: docksStub,
      });
    }).as('getDocks');

    // GET /api/VesselType
    cy.intercept('GET', '**/api/VesselType*', {
      statusCode: 200,
      body: vesselTypesMock,
    }).as('getVesselTypes');

    // POST /api/Dock
    cy.intercept('POST', '**/api/Dock', (req) => {
      const newDock = {
        id: String(docksStub.length + 1),
        ...req.body,
      };

      docksStub = [...docksStub, newDock];

      req.reply({
        statusCode: 201,
        body: newDock,
      });
    }).as('createDock');

    // PUT /api/Dock/:id
    cy.intercept('PUT', '**/api/Dock/*', (req) => {
      const id = req.url.split('/').pop();
      const index = docksStub.findIndex(d => d.id == id);

      if (index < 0) {
        req.reply({ statusCode: 404 });
        return;
      }

      docksStub[index] = {
        ...docksStub[index],
        ...req.body,
      };

      req.reply({
        statusCode: 200,
        body: docksStub[index],
      });
    }).as('updateDock');

    // DELETE /api/Dock/:id
    cy.intercept('DELETE', '**/api/Dock/*', (req) => {
      const id = req.url.split('/').pop();
      docksStub = docksStub.filter(d => d.id != id);

      req.reply({ statusCode: 200 });
    }).as('deleteDock');

    // Visita a página
    cy.visit('/manage-docks');

    // Espera pelas chamadas iniciais
    cy.wait('@authMe');
    cy.wait('@getDocks');
    cy.wait('@getVesselTypes');
  });
});
