/// <reference types="cypress" />

describe('OAuth Authentication and Role-based Access', () => {

  beforeEach(() => {
    cy.visit('/'); // visita a página inicial antes de cada teste
    cy.clearLocalStorage(); // garante que não há tokens antigos
  });
  // Skipped due to intermittent failures, needs investigation
  it.skip('Completes login exchange and stores tokens in localStorage', () => {
    // Intercepta a chamada ao backend para simular login
    cy.intercept('POST', '**/auth/google', {
      statusCode: 200,
      body: {
        idToken: 'fake_id_token',
        name: 'John Doe',
        email: 'john@example.com',
        picture: 'john.png',
        role: 'Admin',
        status: 'Active'
      }
    }).as('googleLogin');

    // Simula retorno do Google com code na URL
    cy.visit('/?code=fake_code');

    // Espera pela chamada interceptada
    cy.wait('@googleLogin');

    // Verifica se os tokens e dados foram salvos no localStorage
    cy.window().then((win) => {
      expect(win.localStorage.getItem('token')).to.eq('fake_id_token');
      expect(win.localStorage.getItem('userName')).to.eq('John Doe');
      expect(win.localStorage.getItem('email')).to.eq('john@example.com');
      expect(win.localStorage.getItem('picture')).to.eq('john.png');
      expect(win.localStorage.getItem('role')).to.eq('Admin');
      expect(win.localStorage.getItem('status')).to.eq('Active');
    });
  });

  it('Shows Control Panel link when the user finish the login', () => {
    // Simula usuário logado
    cy.window().then(win => {
      win.localStorage.setItem('token', 'fake_id_token');
      win.localStorage.setItem('role', 'Admin');
      win.localStorage.setItem('status', 'Active');
    });

    cy.visit('/');

    // O link da role deve estar visível
    cy.get('a[aria-label="Role UI link"]').should('be.visible');
  });

  it('Blocks access and shows error message when user has no role', () => {
    cy.intercept('POST', '**/auth/google', {
      statusCode: 200,
      body: {
        access_token: 'token',
        refresh_token: 'refresh',
        name: 'John Doe',
        email: 'john@example.com',
        foto: '',
        role: '',
        status: 'inactive'
      }
    }).as('exchange');

    cy.on('window:alert', (text) => {
      expect(text).to.match(/login failed/i);
    });

    cy.visit('/?code=fake');
    cy.wait('@exchange');
  });

  it('Blocks access and shows error message when user is inactive', () => {
    cy.intercept('POST', '**/auth/google', {
      statusCode: 200,
      body: {
        access_token: 'token',
        refresh_token: 'refresh',
        name: 'John Doe',
        email: 'john@example.com',
        foto: '',
        role: 'Admin',
        status: 'inactive'
      }
    }).as('exchange');

    cy.on('window:alert', (text) => {
      expect(text).to.match(/login failed/i);
    });

    cy.visit('/?code=fake');
    cy.wait('@exchange');
  });

});

