describe('Manage Users - Edit User', () => {

  const api = Cypress.env('apiUrl');

  const adminUser = {
    email: "admin@test.com",
    name: "Test Admin",
    role: "Admin",
    status: "Active"
  };

  beforeEach(() => {

    cy.intercept('POST', `${api}/auth/google`, {
      statusCode: 200,
      body: {
        access_token: "dummyAccess",
        id_token: "dummyId"
      }
    }).as('googleAuth');

    cy.intercept('POST', `${api}/auth/google/user`, {
      statusCode: 200,
      body: [
        {email: "john@gmail.com", name: "John Doe", role: "Operator", status: "Active"},
        {email: "maria@gmail.com", name: "Maria Silva", role: "ProjectManager", status: "Active"}
      ]
    }).as('userAuth');

    cy.intercept('GET', `${api}/UserManagement/get`, {
      body: [
        {email: "john@gmail.com", name: "John Doe", role: "Operator", status: "Active"},
        {email: "maria@gmail.com", name: "Maria Silva", role: "ProjectManager", status: "Active"}
      ]
    }).as('getUsers');

    cy.visit('/manage-users', {
      onBeforeLoad(win) {
        win.localStorage.setItem('user', JSON.stringify(adminUser));
      }
    });

    cy.wait('@getUsers');
  });

  it('should edit user role', () => {

    cy.contains('td', 'john@gmail.com')
      .parent()
      .find('button')
      .click();

    cy.get('input[name="email"]')
      .should('have.value', 'john@gmail.com')
      .and('be.disabled');

    cy.get('select[name="role"]').select('LogisticsOperator');

    cy.intercept('PUT', `${api}/UserManagement/john@gmail.com/role`, {
      statusCode: 200,
      body: { message: "Role updated." }
    }).as('updateRole');

    cy.get('form').submit();

    cy.wait('@updateRole')
      .its('request.body')
      .should('deep.equal', {
        role: 'LogisticsOperator'
      });

    cy.contains('Usuário atualizado').should('be.visible');
  });
});
