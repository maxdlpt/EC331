import numpy as np
import matplotlib.pyplot as plt

# Parameter Calibration
sigma_e = 0.5         # Automation growth rate
A_crit = 50        # Technology level threshold
kappa = 0.1        # Smoothness of the curve
dt = 0.1           # Time step size
time_steps = 1000  # Number of time steps
A_values = np.linspace(0, 150, time_steps)  # Technology levels over time

# Set total tasks and maximum automation fraction
N_total = 100       # Maximum total tasks
Delta = 0.8         # Maximum fraction automated
max_tasks = Delta * N_total

# Dynamic model simulation with new boundary
N_A_new = np.zeros(time_steps)
for t in range(1, time_steps):
    A_t = A_values[t]
    N_A_dot = A_t**sigma_e * (1 - N_A_new[t - 1] / max_tasks) * (1 - np.exp((kappa*(A_crit - A_t))))
    N_A_dot = max(N_A_dot, 0)
    N_A_new[t] = N_A_new[t-1] + N_A_dot * dt
    if N_A_new[t] > max_tasks:
        N_A_new[t] = max_tasks

# Compute time derivative from dynamic model
N_A_dot_new = np.diff(N_A_new) / dt

# Plot results
plt.figure(figsize=(10, 6))
plt.subplot(2, 1, 1)
plt.plot(A_values[1:], N_A_dot_new, label='$\dot{N}_{A,t}$ (Newly automated tasks)', color='purple')
plt.axvline(x=A_crit, color='black', linestyle='--', label=u'$A_{crit}$')
plt.xlabel(u'$A_t$ (Technology Level)')
plt.ylabel(u'$\dot{N}_{A,t}$')
plt.grid(True)
plt.legend()

plt.subplot(2, 1, 2)
plt.axhline(y=N_total, color='black', linestyle='-', label=u'$N$=100')
plt.axhline(y=max_tasks, color='#565659', linestyle='-', label=u'${\Delta}$ (Max Automated Tasks)')
plt.plot(A_values, N_A_new, label=u'$N_{A,t}$ (# of Automated Tasks)', color='red')
plt.axvline(x=A_crit, color='black', linestyle='--', label=u'$A_{crit}$')
plt.xlabel(u'$A_t$ (Technology Level)')
plt.ylabel(u'$N_{A,t}$')
plt.grid(True)
plt.legend()

plt.tight_layout()
plt.show()
