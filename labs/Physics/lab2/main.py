 
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyArrowPatch
import cmath
import math

# ============================================================================
# 1. РАСЧЁТ ПАРАМЕТРОВ ЦЕПИ
# ============================================================================

# Параметры цепи
U = 220.0          # В, действующее значение напряжения
f = 50.0           # Гц
omega = 2 * math.pi * f  # рад/с

# Параметры элементов
R1 = 9.0           # Ом
C1 = 800e-6        # Ф
R2 = 9.0           # Ом
C2 = 1000e-6       # Ф
L2 = 17e-3         # Гн
C3 = 800e-6        # Ф
R3 = 5.0           # Ом

# Реактивные сопротивления
X_C1 = 1/(omega * C1)  # 3.979 Ом
X_C2 = 1/(omega * C2)  # 3.183 Ом
X_L2 = omega * L2      # 5.341 Ом
X_C3 = 1/(omega * C3)  # 3.979 Ом

# Комплексные сопротивления
Z1 = complex(R1, -X_C1)         # 9 - j3.979 Ом
Z2 = complex(R2, X_L2 - X_C2)   # 9 + j2.158 Ом
Z3 = complex(0, -X_C3)          # -j3.979 Ом
Z4 = complex(R3, 0)             # 5 Ом

# Эквивалентное сопротивление параллельного участка
Y2 = 1 / Z2
Y3 = 1 / Z3
Y4 = 1 / Z4
Y_par = Y2 + Y3 + Y4
Z_par = 1 / Y_par

# Общее сопротивление цепи
Z_total = Z1 + Z_par

# Ток в неразветвленной части
U_complex = complex(U, 0)  # Напряжение с начальной фазой 0°
I1 = U_complex / Z_total
I1_abs = abs(I1)
I1_phase = math.degrees(cmath.phase(I1))

# Напряжение на параллельном участке
U_BC = I1 * Z_par
U_BC_abs = abs(U_BC)
U_BC_phase = math.degrees(cmath.phase(U_BC))

# Токи в параллельных ветвях
I2 = U_BC / Z2
I2_abs = abs(I2)
I2_phase = math.degrees(cmath.phase(I2))

I3 = U_BC / Z3
I3_abs = abs(I3)
I3_phase = math.degrees(cmath.phase(I3))

I4 = U_BC / Z4
I4_abs = abs(I4)
I4_phase = math.degrees(cmath.phase(I4))

# Проверка по первому закону Кирхгофа
I_sum = I2 + I3 + I4

# Напряжение на Z1
U_Z1 = I1 * Z1
U_Z1_abs = abs(U_Z1)
U_Z1_phase = math.degrees(cmath.phase(U_Z1))

# ============================================================================
# 2. ВЫВОД РЕЗУЛЬТАТОВ
# ============================================================================

print("=" * 70)
print("РЕЗУЛЬТАТЫ РАСЧЁТА ЦЕПИ СИНУСОИДАЛЬНОГО ТОКА")
print("=" * 70)

print(f"\nРеактивные сопротивления:")
print(f"  X_C1 = {X_C1:.3f} Ом")
print(f"  X_C2 = {X_C2:.3f} Ом")
print(f"  X_L2 = {X_L2:.3f} Ом")
print(f"  X_C3 = {X_C3:.3f} Ом")

print(f"\nКомплексные сопротивления:")
print(f"  Z1 = {Z1.real:.3f} + j{Z1.imag:.3f} = {abs(Z1):.3f}∠{math.degrees(cmath.phase(Z1)):.2f}°")
print(f"  Z2 = {Z2.real:.3f} + j{Z2.imag:.3f} = {abs(Z2):.3f}∠{math.degrees(cmath.phase(Z2)):.2f}°")
print(f"  Z3 = {Z3.real:.3f} + j{Z3.imag:.3f} = {abs(Z3):.3f}∠{math.degrees(cmath.phase(Z3)):.2f}°")
print(f"  Z4 = {Z4.real:.3f} + j{Z4.imag:.3f} = {abs(Z4):.3f}")

print(f"\nОбщее сопротивление цепи:")
print(f"  Z_общ = {Z_total.real:.3f} + j{Z_total.imag:.3f} = {abs(Z_total):.3f}∠{math.degrees(cmath.phase(Z_total)):.2f}°")

print(f"\nТоки цепи (действующие значения):")
print(f"  I1 = {I1_abs:.3f} А, фаза: {I1_phase:.2f}°")
print(f"  I2 = {I2_abs:.3f} А, фаза: {I2_phase:.2f}°")
print(f"  I3 = {I3_abs:.3f} А, фаза: {I3_phase:.2f}°")
print(f"  I4 = {I4_abs:.3f} А, фаза: {I4_phase:.2f}°")

print(f"\nНапряжения цепи (действующие значения):")
print(f"  U_ист = 220.0 В, фаза: 0.00°")
print(f"  U_BC = {U_BC_abs:.3f} В, фаза: {U_BC_phase:.2f}°")
print(f"  U_Z1 = {U_Z1_abs:.3f} В, фаза: {U_Z1_phase:.2f}°")

print(f"\nПроверка по I закону Кирхгофа:")
print(f"  I2 + I3 + I4 = {I_sum.real:.3f} + j{I_sum.imag:.3f}")
print(f"  I1 = {I1.real:.3f} + j{I1.imag:.3f}")
print(f"  Разница: {abs(I_sum - I1):.6f}")

# ============================================================================
# 3. ПОСТРОЕНИЕ ВЕКТОРНОЙ ДИАГРАММЫ
# ============================================================================

# Создаём фигуру с двумя подграфиками
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))

# ============================================================================
# 3.1 ДИАГРАММА НАПРЯЖЕНИЙ
# ============================================================================


# Масштаб для напряжений: 1 единица = 10 В
scale_U = 0.045  # Масштабный коэффициент для отображения

# Преобразуем комплексные напряжения в декартовы координаты с масштабированием
U_vec = np.array([U_complex.real * scale_U, U_complex.imag * scale_U])
U_BC_vec = np.array([U_BC.real * scale_U, U_BC.imag * scale_U])
U_Z1_vec = np.array([U_Z1.real * scale_U, U_Z1.imag * scale_U])

# Вычисляем сумму U_Z1 + U_BC для проверки
U_sum_vec = U_Z1_vec + U_BC_vec

# Настройка первого графика (напряжения)
ax1.set_xlim(-1.5, 12)
ax1.set_ylim(-4, 4)
ax1.set_aspect('equal', adjustable='box')
ax1.grid(True, linestyle='--', alpha=0.7)
ax1.set_title('Векторная диаграмма напряжений', fontsize=14, fontweight='bold')
ax1.set_xlabel('Re, относительные единицы', fontsize=12)
ax1.set_ylabel('Im, относительные единицы', fontsize=12)

# Рисуем оси
ax1.axhline(y=0, color='black', linewidth=0.5)
ax1.axvline(x=0, color='black', linewidth=0.5)

# Начало координат
origin = np.array([0, 0])

# Вектор напряжения источника U
ax1.arrow(origin[0], origin[1], U_vec[0], U_vec[1], 
          head_width=0.3, head_length=0.4, fc='red', ec='red', linewidth=3, 
          label=f'U = 220 В ∠0°')

# Вектор напряжения на Z1 (сдвигаем от конца U, но для наглядности от начала)
ax1.arrow(origin[0], origin[1], U_Z1_vec[0], U_Z1_vec[1], 
          head_width=0.3, head_length=0.4, fc='blue', ec='blue', linewidth=2,
          label=f'U_Z1 = {U_Z1_abs:.1f} В ∠{U_Z1_phase:.1f}°')

# Вектор напряжения U_BC (от конца U_Z1)
ax1.arrow(U_Z1_vec[0], U_Z1_vec[1], U_BC_vec[0], U_BC_vec[1], 
          head_width=0.3, head_length=0.4, fc='green', ec='green', linewidth=2,
          label=f'U_BC = {U_BC_abs:.1f} В ∠{U_BC_phase:.1f}°')

# Пунктирная линия для суммы U_Z1 + U_BC (должна совпадать с U)
ax1.arrow(origin[0], origin[1], U_sum_vec[0], U_sum_vec[1], 
          head_width=0.3, head_length=0.4, fc='black', ec='black', 
          linewidth=1, linestyle='--', alpha=0.7,
          label='U_Z1 + U_BC (проверка)')

# Добавляем углы
# Угол напряжения U (0°)
ax1.text(1, 0.2, '0°', fontsize=10, color='red')

# Угол напряжения U_Z1
angle_U_Z1 = math.atan2(U_Z1_vec[1], U_Z1_vec[0])
ax1.text(U_Z1_vec[0]/2 + 0.3, U_Z1_vec[1]/2, 
         f'{U_Z1_phase:.1f}°', fontsize=10, color='blue')

# Угол напряжения U_BC (относительно начала координат, но покажем относительно U_Z1)
angle_U_BC_total = math.atan2(U_Z1_vec[1] + U_BC_vec[1], U_Z1_vec[0] + U_BC_vec[0])
ax1.text(U_Z1_vec[0] + U_BC_vec[0]/2 - 0.5, U_Z1_vec[1] + U_BC_vec[1]/2 + 0.3, 
         f'{U_BC_phase:.1f}°', fontsize=10, color='green')

ax1.legend(loc='upper right', fontsize=10)
ax1.set_facecolor('#f8f9fa')

# ============================================================================
# 3.2 ДИАГРАММА ТОКОВ
# ============================================================================

# Масштаб для токов: 1 единица = 2 А
scale_I = 0.11  # Масштабный коэффициент для отображения

# Преобразуем комплексные токи в декартовы координаты с масштабированием
I1_vec = np.array([I1.real * scale_I, I1.imag * scale_I])
I2_vec = np.array([I2.real * scale_I, I2.imag * scale_I])
I3_vec = np.array([I3.real * scale_I, I3.imag * scale_I])
I4_vec = np.array([I4.real * scale_I, I4.imag * scale_I])

# Вычисляем сумму I2 + I3 + I4 для проверки
I_sum_vec = I2_vec + I3_vec + I4_vec

# Настройка второго графика (токи)
ax2.set_xlim(-2, 12)
ax2.set_ylim(-2, 10)
ax2.set_aspect('equal', adjustable='box')
ax2.grid(True, linestyle='--', alpha=0.7)
ax2.set_title('Векторная диаграмма токов', fontsize=14, fontweight='bold')
ax2.set_xlabel('Re, относительные единицы', fontsize=12)
ax2.set_ylabel('Im, относительные единицы', fontsize=12)

# Рисуем оси
ax2.axhline(y=0, color='black', linewidth=0.5)
ax2.axvline(x=0, color='black', linewidth=0.5)

# Вектор тока I1
ax2.arrow(origin[0], origin[1], I1_vec[0], I1_vec[1], 
          head_width=0.3, head_length=0.4, fc='darkred', ec='darkred', linewidth=3,
          label=f'I1 = {I1_abs:.2f} А ∠{I1_phase:.1f}°')


# Вектор тока I2
ax2.arrow(origin[0], origin[1], I2_vec[0], I2_vec[1], 
          head_width=0.2, head_length=0.3, fc='purple', ec='purple', linewidth=2,
          label=f'I2 = {I2_abs:.2f} А ∠{I2_phase:.1f}°')

# Вектор тока I3
ax2.arrow(origin[0], origin[1], I3_vec[0], I3_vec[1], 
          head_width=0.2, head_length=0.3, fc='orange', ec='orange', linewidth=2,
          label=f'I3 = {I3_abs:.2f} А ∠{I3_phase:.1f}°')

# Вектор тока I4
ax2.arrow(origin[0], origin[1], I4_vec[0], I4_vec[1], 
          head_width=0.2, head_length=0.3, fc='brown', ec='brown', linewidth=2,
          label=f'I4 = {I4_abs:.2f} А ∠{I4_phase:.1f}°')

# Пунктирная линия для суммы I2 + I3 + I4 (должна совпадать с I1)
ax2.arrow(origin[0], origin[1], I_sum_vec[0], I_sum_vec[1], 
          head_width=0.3, head_length=0.4, fc='black', ec='black', 
          linewidth=1, linestyle='--', alpha=0.7,
          label='I2 + I3 + I4 (проверка)')

# Добавляем углы
# Угол тока I1
angle_I1 = math.atan2(I1_vec[1], I1_vec[0])
ax2.text(I1_vec[0]/2 + 0.5, I1_vec[1]/2, 
         f'{I1_phase:.1f}°', fontsize=10, color='darkred')

# Угол тока I2
angle_I2 = math.atan2(I2_vec[1], I2_vec[0])
ax2.text(I2_vec[0]/2 - 0.8, I2_vec[1]/2 - 0.3, 
         f'{I2_phase:.1f}°', fontsize=10, color='purple')

# Угол тока I3
angle_I3 = math.atan2(I3_vec[1], I3_vec[0])
ax2.text(I3_vec[0]/2 - 1.0, I3_vec[1]/2 + 0.5, 
         f'{I3_phase:.1f}°', fontsize=10, color='orange')

# Угол тока I4
angle_I4 = math.atan2(I4_vec[1], I4_vec[0])
ax2.text(I4_vec[0]/2 + 0.3, I4_vec[1]/2 - 0.5, 
         f'{I4_phase:.1f}°', fontsize=10, color='brown')

ax2.legend(loc='upper right', fontsize=10)
ax2.set_facecolor('#f8f9fa')

# ============================================================================
# 3.3 ТАБЛИЦА С РЕЗУЛЬТАТАМИ
# ============================================================================

# Создаём третью фигуру для таблицы
fig2, ax3 = plt.subplots(figsize=(12, 8))
ax3.axis('tight')
ax3.axis('off')

# Данные для таблицы
data = [
    ["Параметр", "Значение", "Единица измерения", "Примечание"],
    ["Напряжение источника", "220.00", "В", "U ∠0°"],
    ["Частота", "50.00", "Гц", "f = 50 Гц"],
    ["Общий ток I₁", f"{I1_abs:.3f} ∠{I1_phase:.2f}°", "А", "Ток в неразветвленной части"],
    ["Ток в ветви 1 (I₂)", f"{I2_abs:.3f} ∠{I2_phase:.2f}°", "А", "R₂-C₂-L₂ ветвь"],
    ["Ток в ветви 2 (I₃)", f"{I3_abs:.3f} ∠{I3_phase:.2f}°", "А", "C₃ ветвь"],
    ["Ток в ветви 3 (I₄)", f"{I4_abs:.3f} ∠{I4_phase:.2f}°", "А", "R₃ ветвь"],
    ["Напряжение U_BC", f"{U_BC_abs:.3f} ∠{U_BC_phase:.2f}°", "В", "Напряжение на параллельном участке"],
    ["Сопротивление Z₁", f"{abs(Z1):.3f} ∠{math.degrees(cmath.phase(Z1)):.2f}°", "Ом", "R₁-C₁ последовательно"],
    ["Сопротивление Z₂", f"{abs(Z2):.3f} ∠{math.degrees(cmath.phase(Z2)):.2f}°", "Ом", "R₂-C₂-L₂ последовательно"],
    ["Сопротивление Z₃", f"{abs(Z3):.3f} ∠{math.degrees(cmath.phase(Z3)):.2f}°", "Ом", "C₃"],
    ["Сопротивление Z₄", f"{abs(Z4):.3f}", "Ом", "R₃"],
    ["Общее сопротивление", f"{abs(Z_total):.3f} ∠{math.degrees(cmath.phase(Z_total)):.2f}°", "Ом", "Z₁ + Z_пар"],
    ["Коэффициент мощности", f"{math.cos(math.radians(I1_phase)):.3f}", "", f"cos({I1_phase:.1f}°)"]
]

# Создаём таблицу
table = ax3.table(cellText=data, loc='center', cellLoc='center', colWidths=[0.25, 0.2, 0.2, 0.35])

# Настраиваем таблицу
table.auto_set_font_size(False)
table.set_fontsize(10)
table.scale(1.2, 1.8)

# Цвета для заголовка
for i in range(4):
    table[(0, i)].set_facecolor("#4B8BBE")
    table[(0, i)].set_text_props(weight='bold', color='white')

# Чередующиеся цвета строк
for i in range(1, len(data)):
    for j in range(4):
        if i % 2 == 0:
            table[(i, j)].set_facecolor("#F0F0F0")
        else:
            table[(i, j)].set_facecolor("#FFFFFF")

ax3.set_title('Сводная таблица результатов расчёта цепи', fontsize=14, fontweight='bold', pad=20)

# ============================================================================
# 4. СОХРАНЕНИЕ И ОТОБРАЖЕНИЕ
# ============================================================================


plt.tight_layout()

# Сохраняем диаграммы
fig.savefig('vector_diagram.png', dpi=300, bbox_inches='tight')
fig2.savefig('results_table.png', dpi=300, bbox_inches='tight')

print(f"\n{'='*70}")
print("Векторная диаграмма сохранена как 'vector_diagram.png'")
print("Таблица результатов сохранена как 'results_table.png'")
print(f"{'='*70}")

# Показываем графики
plt.show()

# ============================================================================
# 5. ДОПОЛНИТЕЛЬНАЯ ИНФОРМАЦИЯ
# ============================================================================

print(f"\nДополнительная информация:")
print(f"  Модуль Z_пар = {abs(Z_par):.3f} Ом")
print(f"  Аргумент Z_пар = {math.degrees(cmath.phase(Z_par)):.2f}°")
print(f"  Проводимость Y_пар = {Y_par.real:.4f} + j{Y_par.imag:.4f} См")
print(f"\nСоотношение фаз:")
print(f"  U опережает I1 на {abs(I1_phase):.2f}° (ёмкостной характер)")
print(f"  U_BC отстаёт от U на {abs(U_BC_phase):.2f}°")
print(f"  I3 опережает U_BC на {abs(90 - U_BC_phase):.2f}° (чисто ёмкостной характер)")

# Рассчитываем мощности для баланса
P1 = I1_abs**2 * R1
P2 = I2_abs**2 * R2
P4 = I4_abs**2 * R3
P_total = P1 + P2 + P4

Q_C1 = -I1_abs**2 * X_C1
Q_L2 = I2_abs**2 * X_L2
Q_C2 = -I2_abs**2 * X_C2
Q_C3 = -I3_abs**2 * X_C3
Q_total = Q_C1 + Q_L2 + Q_C2 + Q_C3

S = U * I1_abs
S_calc = math.sqrt(P_total**2 + Q_total**2)
cos_phi = P_total / S

print(f"\nБаланс мощностей:")
print(f"  Активная мощность P = {P_total:.1f} Вт")
print(f"  Реактивная мощность Q = {Q_total:.1f} вар")
print(f"  Полная мощность S = {S:.1f} ВА")
print(f"  Расчётная S = {S_calc:.1f} ВА")
print(f"  Коэффициент мощности cos φ = {cos_phi:.3f}")
print(f"  Погрешность баланса: {abs(S - S_calc)/S*100:.2f}%")
