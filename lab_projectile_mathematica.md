# Виртуальная лабораторная работа
## Тема: Исследование кинематики тела, брошенного под углом к горизонту, с учётом сопротивления воздуха (Wolfram Mathematica)

---

## 1. Цель работы

1. Построить **интерактивную** модель движения тела, брошенного под углом к горизонту.
2. Управлять параметрами модели в реальном времени: \(v_0\), \(\alpha\), \(g\), \(y_0\), \(k\).
3. Сравнивать траектории **без сопротивления** и **с сопротивлением воздуха**.
4. Наблюдать влияние сопротивления воздуха на время полёта, дальность и максимальную высоту.
5. Использовать фиксированный масштаб графиков и ручные регуляторы масштаба для корректного визуального сравнения.

---

## 2. Краткая теория

### 2.1. Без сопротивления воздуха

\[
\dot x = v_x,\quad \dot y = v_y,\quad \dot v_x = 0,\quad \dot v_y = -g.
\]

### 2.2. С линейным сопротивлением воздуха

Введём коэффициент сопротивления \(k\) (в 1/с). Тогда:

\[
\dot x = v_x,
\quad
\dot y = v_y,
\quad
\dot v_x = -k v_x,
\quad
\dot v_y = -g-k v_y.
\]

Начальные условия:

\[
x(0)=0,\; y(0)=y_0,\; v_x(0)=v_0\cos\alpha,\; v_y(0)=v_0\sin\alpha.
\]

При \(k=0\) получаем классическую модель без сопротивления.

---

## 3. Что делает виртуальная лаборатория

После запуска кода вы получаете:

- ползунки для изменения \(v_0\), \(\alpha\), \(g\), \(y_0\), \(k\);
- анимацию полёта точки по траектории;
- вектор скорости в текущий момент времени;
- графики \(x(t)\), \(y(t)\), \(v_y(t)\);
- численный расчёт \(T\), \(H_{\max}\), \(L\) с учётом сопротивления;
- фиксированный масштаб графиков и отдельные регуляторы масштаба;
- возможность поставить \(k=0\) и сравнить с идеальным случаем.

---

## 4. Код виртуальной лабораторной работы (Wolfram Mathematica)

> Скопируйте код в ноутбук Mathematica и выполните целиком.

```wolfram
ClearAll["Global`*"];

(* ===== Численное моделирование с линейным сопротивлением ===== *)
solutionFunctions[v0_, α_, g_, y0_, k_, tMax_] :=
 NDSolveValue[
  {
   x'[t] == vx[t],
   y'[t] == vy[t],
   vx'[t] == -k vx[t],
   vy'[t] == -g - k vy[t],
   x[0] == 0,
   y[0] == y0,
   vx[0] == v0 Cos[α],
   vy[0] == v0 Sin[α]
   },
  {x, y, vx, vy},
  {t, 0, tMax},
  MaxStepFraction -> 1/200
  ];

flightTimeFromY[yf_, tMax_] :=
 Module[{root},
  root = Quiet@Check[
     t /. FindRoot[yf[t] == 0, {t, 10^-4, tMax}],
     Missing["NoRoot"]
     ];
  If[NumericQ[root] && 0 <= root <= tMax, root, tMax]
  ];

Manipulate[
 Module[
  {
   xF, yF, vxF, vyF,
   T, H, L,
   tNow, xt, yt, vxt, vyt, speed,
   tGrid, trajPlot, pointPlot,
   graphX, graphY, graphVy,
   rangePlot, rangeTable, anglesDeg,
   xTop, yTop, tPlotMax, vyTop,
   tRootMax
   },

  tRootMax = Max[2, tSolveMax];

  {xF, yF, vxF, vyF} = solutionFunctions[v0, α, g, y0, kDrag, tRootMax];

  T = flightTimeFromY[yF, tRootMax];
  H = Quiet@Check[NMaxValue[{yF[t], 0 <= t <= T}, t], y0];
  L = Quiet@Check[xF[T], 0];

  tNow = Min[tAnim, T];
  xt = xF[tNow];
  yt = yF[tNow];
  vxt = vxF[tNow];
  vyt = vyF[tNow];
  speed = Sqrt[vxt^2 + vyt^2];

  xTop = Max[10, xWindow];
  yTop = Max[5, yWindow];
  tPlotMax = Max[1, tWindow];
  vyTop = Max[5, vyWindow];

  tGrid = Subdivide[0, T, 250];

  trajPlot = ListLinePlot[
    Transpose[{xF /@ tGrid, yF /@ tGrid}],
    PlotStyle -> {Thick, Blue},
    AxesLabel -> {"x, м", "y, м"},
    PlotLabel -> "Траектория и текущее положение",
    GridLines -> Automatic,
    PlotRange -> {{0, xTop}, {0, yTop}},
    ImageSize -> 520
    ];

  pointPlot = Graphics[
    {
     Red, PointSize[0.02], Point[{xt, yt}],
     Darker@Green,
     Arrow[{{xt, yt}, {xt + arrowScale vxt, yt + arrowScale vyt}}],
     Black,
     Inset[
      Style[Row[{"v = ", NumberForm[speed, {5, 2}], " м/с"}], 12, Bold],
      {0.80 xTop, 0.90 yTop}
      ]
     }
    ];

  graphX = Plot[
    xF[t], {t, 0, T},
    PlotStyle -> Red,
    PlotLabel -> "x(t)",
    AxesLabel -> {"t, c", "x, м"},
    PlotRange -> {{0, tPlotMax}, {0, xTop}},
    GridLines -> Automatic,
    ImageSize -> 250
    ];

  graphY = Plot[
    yF[t], {t, 0, T},
    PlotStyle -> Darker@Blue,
    PlotLabel -> "y(t)",
    AxesLabel -> {"t, c", "y, м"},
    PlotRange -> {{0, tPlotMax}, {0, yTop}},
    GridLines -> Automatic,
    ImageSize -> 250
    ];

  graphVy = Plot[
    vyF[t], {t, 0, T},
    PlotStyle -> Purple,
    PlotLabel -> "vy(t)",
    AxesLabel -> {"t, c", "vy, м/с"},
    PlotRange -> {{0, tPlotMax}, {-vyTop, vyTop}},
    GridLines -> Automatic,
    ImageSize -> 250
    ];

  anglesDeg = Range[5, 85, 5];
  rangeTable = Table[
    Module[{solA, TA, LA},
     solA = solutionFunctions[v0, θ Degree, g, y0, kDrag, tRootMax];
     TA = flightTimeFromY[solA[[2]], tRootMax];
     LA = Quiet@Check[solA[[1]][TA], 0];
     {θ, N@LA}
     ],
    {θ, anglesDeg}
    ];

  rangePlot = ListLinePlot[
    rangeTable,
    PlotStyle -> {Thick, Orange},
    PlotMarkers -> Automatic,
    AxesLabel -> {"Угол, град", "Дальность, м"},
    PlotLabel -> "Зависимость дальности от угла (с сопротивлением)",
    GridLines -> Automatic,
    PlotRange -> {{0, 90}, {0, xTop}},
    ImageSize -> 520,
    Epilog -> {
      Red, PointSize[0.02], Point[{α/Degree, N@L}],
      Black,
      Text[
       Style[Row[{"текущий угол = ", NumberForm[α/Degree, {3, 1}], "°"}], 11, Bold],
       {Min[88, α/Degree + 5], Min[0.95 xTop, N@L + 0.05 xTop]}
       ]
      }
    ];

  Column[
   {
    Style["Виртуальная лаборатория: бросок под углом (с сопротивлением воздуха)", 15, Bold],

    Grid[
     {
      {"Коэффициент сопротивления k (1/с)", NumberForm[kDrag, {5, 3}]},
      {"Время полёта T (с)", NumberForm[T, {7, 3}]},
      {"Максимальная высота Hmax (м)", NumberForm[H, {7, 3}]},
      {"Дальность L (м)", NumberForm[L, {7, 3}]},
      {"Текущее время t (с)", NumberForm[tNow, {7, 3}]},
      {"Текущие координаты (x, y)",
       Row[{"(", NumberForm[xt, {6, 2}], ", ", NumberForm[yt, {6, 2}], ")"}]}
      },
     Frame -> All,
     ItemSize -> All,
     Background -> {None, {{Lighter[Gray, 0.93], White}}}
     ],

    Show[trajPlot, pointPlot],
    GraphicsRow[{graphX, graphY, graphVy}, Spacings -> 10],
    rangePlot
    },
   Spacings -> 1.2
   ]
  ],

 (* ---------- Параметры полёта ---------- *)
 {{v0, 20, "Начальная скорость v0 (м/с)"}, 1, 80, 1, Appearance -> "Labeled"},
 {{α, 45 Degree, "Угол броска α (град)"}, 1 Degree, 89 Degree, 1 Degree, Appearance -> "Labeled"},
 {{g, 9.81, "Ускорение g (м/с^2)"}, 1.6, 24.8, 0.01, Appearance -> "Labeled"},
 {{y0, 0, "Начальная высота y0 (м)"}, 0, 50, 0.5, Appearance -> "Labeled"},
 {{kDrag, 0.00, "Сопротивление воздуха k (1/с)"}, 0, 0.60, 0.01, Appearance -> "Labeled"},
 {{tSolveMax, 20, "Максимальное время расчёта (с)"}, 2, 80, 1, Appearance -> "Labeled"},

 Delimiter,

 (* ---------- Масштаб (ручной, фиксированный) ---------- *)
 {{xWindow, 60, "Масштаб по X (м)"}, 10, 300, 5, Appearance -> "Labeled"},
 {{yWindow, 30, "Масштаб по Y (м)"}, 5, 200, 1, Appearance -> "Labeled"},
 {{tWindow, 8, "Масштаб по времени для x(t), y(t), vy(t) (с)"}, 1, 40, 0.5, Appearance -> "Labeled"},
 {{vyWindow, 40, "Масштаб vy: от -V до +V (м/с)"}, 5, 150, 1, Appearance -> "Labeled"},
 {{arrowScale, 0.12, "Масштаб стрелки скорости"}, 0.02, 0.40, 0.01, Appearance -> "Labeled"},

 Delimiter,

 (* ---------- Анимация ---------- *)
 {{tAnim, 0, "Время анимации t (с)"}, 0, Dynamic[tSolveMax],
  Animator,
  AnimationRunning -> False,
  AnimationRate -> 0.8,
  AppearanceElements -> {"ProgressSlider", "PlayPauseButton", "FasterSlowerButtons", "DirectionButton"}
  },

 TrackedSymbols :> {v0, α, g, y0, kDrag, tSolveMax, xWindow, yWindow, tWindow, vyWindow, arrowScale, tAnim},
 SaveDefinitions -> True
 ]
```

---

## 5. Инструкция по выполнению лабораторной

1. Запустите интерактивный модуль.
2. Установите базовые параметры: \(v_0=20\) м/с, \(\alpha=45^\circ\), \(g=9.81\), \(y_0=0\).
3. Поставьте `k = 0` и зафиксируйте значения \(T\), \(H_{\max}\), \(L\).
4. Постепенно увеличивайте `k` (например 0.05, 0.10, 0.20, 0.30) и наблюдайте, как уменьшаются дальность и высота.
5. Для сравнения опытов удерживайте одинаковые значения регуляторов масштаба.
6. При необходимости увеличьте `Максимальное время расчёта`, если траектория при малых скоростях долго падает на землю.

---

## 6. Таблица для отчёта (шаблон)

| № опыта | \(v_0\), м/с | \(\alpha\), град | \(y_0\), м | \(g\), м/с² | \(k\), 1/с | \(T\), с | \(H_{\max}\), м | \(L\), м |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 20 | 45 | 0 | 9.81 | 0.00 |  |  |  |
| 2 | 20 | 45 | 0 | 9.81 | 0.05 |  |  |  |
| 3 | 20 | 45 | 0 | 9.81 | 0.10 |  |  |  |
| 4 | 20 | 45 | 0 | 9.81 | 0.20 |  |  |  |
| 5 | 20 | 45 | 0 | 9.81 | 0.30 |  |  |  |

---

## 7. Контрольные вопросы

1. Почему при увеличении коэффициента сопротивления \(k\) дальность полёта уменьшается?
2. Как изменяется график \(v_y(t)\) при росте \(k\)?
3. Почему при \(k=0\) модель переходит к классической параболе?
4. Какие ограничения имеет модель линейного сопротивления?
5. Почему полезно сравнивать опыты в одинаковом масштабе графиков?

---

## 8. Вывод (пример)

В виртуальной лаборатории исследовано движение тела, брошенного под углом к горизонту, с учётом линейного сопротивления воздуха. Численное моделирование показало, что при увеличении коэффициента сопротивления уменьшаются максимальная высота, дальность и время активного набора высоты. Фиксированный масштаб графиков позволил корректно сравнить траектории и наглядно оценить влияние сопротивления.
