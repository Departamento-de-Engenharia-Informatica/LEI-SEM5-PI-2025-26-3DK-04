describe('Admin - Create User', () => {

  beforeEach(() => {
    cy.loginAsAdmin();
  });

  it('should create a new user successfully', () => {

    const apiBase = 'https://localhost:5001/api';

    // Interceptar lista de users
    cy.intercept('GET', `${apiBase}/UserManagement/get`, {
      statusCode: 200,
      body: [
        {
          id: "1",
          email: "existing@test.com",
          role: "Admin",
          status: "Active"
        }
      ]
    }).as('getUsers');


    // Interceptar criação de utilizador
    cy.intercept('POST', `${apiBase}/UserManagement/create`, (req) => {

      // Verificar header Authorization
      expect(req.headers['authorization']).to.exist;
      expect(req.headers['authorization']).to.contain("Bearer");

      req.reply({
        statusCode: 201,
        body: {
          id: "new-id-123",
          email: req.body.email,
          role: req.body.role
        }
      });

    }).as('createUser');




    ////////////////////////////////
    // UI FLOW
    ////////////////////////////////

    cy.visit('/admin/users');

    cy.wait('@getUsers');

    cy.contains("Create User").click();

    cy.get('input[name="email"]').type("newuser@test.com");
    cy.get('select[name="role"]').select("Admin");

    cy.contains("Submit").click();

    cy.wait('@createUser');

    cy.contains("User created successfully").should('exist');
  });
});
