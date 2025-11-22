describe('Manage Users - Create User', () => {

  const api = Cypress.env('apiUrl');

  // Usuário admin fake para testes
  const adminUser = {
    email: "admin@test.com",
    name: "Test Admin",
    role: "Admin",
    status: "Active",
    token: "dummyIdToken" // importante para o interceptor
  };

  beforeEach(() => {
    // MOCK GOOGLE AUTH
    cy.intercept('POST', `${api}/auth/google`, {
      statusCode: 200,
      body: {
        access_token: "dummyAccess",
        id_token: "dummyIdToken"
      }
    }).as('googleAuth');

    // MOCK USER AUTH
    cy.intercept('POST', `${api}/auth/google/user`, {
      statusCode: 200,
      body: adminUser
    }).as('userAuth');

    // MOCK users list
    cy.intercept('GET', `${api}/UserManagement/get`, {
      statusCode: 200,
      body: [
        {email: "john@gmail.com", name: "John Doe", role: "Operator", status: "Active"},
        {email: "maria@gmail.com", name: "Maria Silva", role: "ProjectManager", status: "Active"}
      ]
    }).as('getUsers');

    // MOCK GET by email para criar usuário
    cy.intercept('GET', `${api}/UserManagement/get/novo.user@gmail.com`, {
      statusCode: 200,
      body: []
    }).as('checkEmail');

    // MOCK create user
    cy.intercept('POST', `${api}/UserManagement/create`, {
      statusCode: 200,
      body: {message: "User created. Activation email sent."}
    }).as('createUser');

    // VISIT página e garante que o usuário está no localStorage **antes do Angular iniciar**
    cy.visit('/manage-users', {
      onBeforeLoad(win) {
        win.localStorage.setItem('user', JSON.stringify(adminUser));
      }
    });

    // Garantir que a requisição GET users seja disparada
    cy.visit('/manage-users', {
      onBeforeLoad(win) {
        win.localStorage.setItem('user', JSON.stringify(adminUser));
      }
    });

// Espera pelo elemento que mostra a lista de usuários
    cy.get('table#users').should('exist');

// Agora espera pelo GET users
    cy.wait('@getUsers');

    it('should create a new user', () => {
      cy.get('input[name="email"]').type('novo.user@gmail.com').blur();
      cy.wait('@checkEmail');

      cy.get('input[name="name"]').type('Novo User');
      cy.get('select[name="role"]').select('ProjectManager');

      cy.get('form').submit();

      cy.wait('@createUser').its('request.body').should('deep.equal', {
        email: 'novo.user@gmail.com',
        name: 'Novo User',
        role: 'ProjectManager'
      });

      cy.contains('Usuário criado').should('be.visible');
    });
  });
});
