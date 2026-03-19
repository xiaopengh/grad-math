# -*- coding: utf-8 -*-
"""
Created on Mon Feb 23 14:02:54 2026

@author: zhanxi240
"""
#%%
import numpy as np
import matplotlib.pyplot as plt

#%%
class OdeSchemes:
    
    def __init__(self, f, X0, T0, T, h):
        
        self.f = f                      # expects f(t, X)
        self.X0 = np.asarray(X0, dtype=float)
        self.T0 = T0
        self.T = T
        self.h = h
        
        self.N = int(np.round((T - T0)/h))
        self.t = self.T0 + np.arange(self.N + 1) * self.h
        self.dim = self.X0.size
    
    def useOdeint(self):
        sol = odeint(self.f, self.X0, self.t, tfirst=True)
        return self.t, sol
        
    def EE(self):
        sol = np.zeros((self.N + 1, self.dim))
        sol[0] = self.X0
        
        for k in range(self.N):
            sol[k+1] = sol[k] + self.h * self.f(self.t[k], sol[k])
        
        return self.t, sol

#%% EXO 2.17
T=1.0
N=100
h = T/N

U0exact=0.0
U0=np.sqrt(2)**2 - 2

Uexact = np.zeros(N+1)
Uexact[0]=U0exact

U = np.zeros(N+1)
U[0]=U0

#implementation Euler Explicite
for k in range(N):
    U[k+1] = U[k] + h * 2 * abs(U[k])**(1/2) 
    Uexact[k+1] = Uexact[k] + h * 2 * abs(Uexact[k])**(1/2) 

plt.figure(1)
plt.plot(np.linspace(0,T,N+1),U,
         np.linspace(0,T,N+1),Uexact)
plt.legend(['precision finie','precision infinie'])
plt.show()

#%% EXO 2.19
T = 100 * 2 * np.pi
h = 2 * np.pi / 100 
N = int(T // h) + 1

#%% EE
solEE = np.zeros((N+1, 2))
solEE[0,:] = 1, 0

def f(t, z):
    """
    input complex z as (2,) ndarray
    output complex iz as (2,) ndarray
    """
    x, y = z
    return np.array([-y, x])

for k in range(N):
    solEE[k+1,:] = solEE[k,:] + h * f(k*h, solEE[k,:])
    
plt.figure()
plt.plot(solEE[:, 0], solEE[:, 1])

plt.xlabel("Real part")
plt.ylabel("Imaginary part")
plt.title("Phase Plot (Explicit Euler)")
plt.grid()
plt.axis("equal")   # important for complex trajectories
plt.show()

#%% IE
solIE = np.zeros((N+1, 2))
solIE[0,:] = 1, 0

for k in range(N):
    y = solIE[k, :]
    for _ in range(100):
        y = solIE[k,:] + h * f(k*h, y)
    solIE[k+1,:] = y

plt.figure()
plt.plot(solIE[:, 0], solIE[:, 1])

plt.xlabel("Real part")
plt.ylabel("Imaginary part")
plt.title("Phase Plot (Implicit Euler)")
plt.grid()
plt.axis("equal")   # important for complex trajectories
plt.show()
#%% EXO 2.18 part odeint
from scipy.integrate import odeint

S0 = 1e6
I0 = 10
R0 = 0
beta_cst = 1./2.
gamma_cst = 1./3.

T = 150
N = 150
t = np.linspace(0, 150, N+1)

def sir_f(t, X):
    """
    input X as (3,) ndarray
    output dX as (3,) ndarray
    """
    S, I, R = X
    return(np.array([-beta_cst*S*I, beta_cst*S*I - gamma_cst*I, gamma_cst*I]))
    
X0 = np.array([S0, I0, R0])
Npop = sum(X0)
X0 = X0 / Npop
sol = odeint(f, X0, t, tfirst=True)

plt.figure(figsize=(8,5))

plt.plot(t, sol[:,0], label="Susceptible")
plt.plot(t, sol[:,1], label="Infected")
plt.plot(t, sol[:,2], label="Recovered")

plt.xlabel("Time")
plt.ylabel("Population")
plt.title("SIR Model (odeint)")
plt.legend(['S', 'I', 'R'])
plt.grid()
plt.show()

#%% EXO 2.18 part order of schemes
idx = range(52, 61)
plt.figure(figsize=(8,5))

plt.plot(t[idx], sol[idx,0], label="Susceptible")
plt.plot(t[idx], sol[idx,1], label="Infected")
plt.plot(t[idx], sol[idx,2], label="Recovered")

plt.xlabel("Time")
plt.ylabel("Population")
plt.title("SIR Model (odeint)")
plt.legend(['S', 'I', 'R'])
plt.grid()
plt.show()

hlist = [0.05, 0.01, 0.1, 0.5, 1, 2, 4]
schemes = []
for h in hlist:
    scheme = OdeSchemes(f = sir_f, X0 = sol[52], T0 = 52, T = 60, h = h)
    schemes.append(scheme)

for i in range(len(schemes)):
    
    t, sol_scheme = schemes[i].EE()
    
    plt.figure(figsize=(8,5))

    plt.plot(t, sol_scheme[:,0], label="Susceptible")
    plt.plot(t, sol_scheme[:,1], label="Infected")
    plt.plot(t, sol_scheme[:,2], label="Recovered")

    plt.xlabel("Time")
    plt.ylabel("Population")
    plt.title(f"SIR Model EE with h = {hlist[i]:.2f}")
    plt.legend(['S', 'I', 'R'])
    plt.grid()
    plt.show()
    



