// ***********************************************
// This example namespace declaration will help
// with Intellisense and code completion in your
// IDE or Text Editor.
// ***********************************************
// declare namespace Cypress {
//   interface Chainable<Subject = any> {
//     customCommand(param: any): typeof customCommand;
//   }
// }
//
// function customCommand(param: any): void {
//   console.warn(param);
// }
//
// NOTE: You can use it like so:
// Cypress.Commands.add('customCommand', customCommand);
//
// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })
// cypress/support/commands.ts

// cypress/support/commands.ts

declare global {
  namespace Cypress {
    interface Chainable {
      loginAsAdmin(): Chainable<void>;
    }
  }
}

const apiBase = 'https://localhost:5001/api';


Cypress.Commands.add('loginAsAdmin', () => {

  // Interceptar login Google (o frontend chama isto ANTES de montar a app)
  cy.intercept('POST', `${apiBase}/auth/google`, {
    statusCode: 200,
    body: {
      token: "mock-jwt-token",
      email: "admin@test.com",
    }
  }).as('googleAuth');

  // Interceptar validação de user (AuthService → get user info)
  cy.intercept('POST', `${apiBase}/auth/google/user`, {
    statusCode: 200,
    body: {
      id: "admin-id-123",
      name: "Test Admin",
      email: "admin@test.com",
      role: "Admin",
      status: "Active"
    }
  }).as('googleUser');


  ////////////////////////////////////////////
  // CRIAR JWT FALSO QUE O BACKEND ACEITA
  ////////////////////////////////////////////

  const header = btoa(JSON.stringify({ alg: "HS256", typ: "JWT" }));
  const payload = btoa(JSON.stringify({
    email: "admin@test.com",     // 🔥 O .NET USA ESTE CAMPO
    role: "Admin",               // usado para autorizar chamadas
    exp: Math.floor(Date.now() / 1000) + (60 * 60 * 24),
  }));
  const signature = "mock";
  const jwt = `${header}.${payload}.${signature}`;


  ////////////////////////////////////////////
  // SESSÃO CYPRESS (mantém login entre tests)
  ////////////////////////////////////////////

  cy.session("admin-session", () => {

      cy.visit("http://localhost:4200");

      cy.window().then((win) => {
        win.localStorage.setItem("jwt", jwt);
        win.localStorage.setItem("user", JSON.stringify({
          id: "admin-id-123",
          email: "admin@test.com",
          name: "Test Admin",
          role: "Admin",
          status: "Active"
        }));
      });

      cy.setCookie("access_token", jwt);
    },
    {
      validate() {
        cy.window().its('localStorage').invoke('getItem', 'jwt').should('exist');
        cy.getCookie('access_token').should('exist');
      },
      cacheAcrossSpecs: true
    });

  cy.log("✔ Login simulado como Admin");
});

export {};
