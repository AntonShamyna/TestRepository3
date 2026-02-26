# Виртуальная лабораторная работа
## Тема: Исследование кинематики тела, брошенного под углом к горизонту, с учётом сопротивления воздуха (Wolfram Mathematica)

---

## 1. Цель работы

1. Построить **интерактивную** модель движения тела, брошенного под углом к горизонту.
2. Управлять параметрами в реальном времени: \(v_0\), \(\alpha\), \(g\), \(y_0\), \(k\).
3. Сравнивать траектории без сопротивления и с сопротивлением воздуха.
4. Сделать интерфейс наглядным и визуально «игровым»: тёмная тема, HUD-панель, цель, цветовые акценты.

---

## 2. Краткая теория

### 2.1. Без сопротивления воздуха

\[
\dot x = v_x,\quad \dot y = v_y,\quad \dot v_x = 0,\quad \dot v_y = -g.
\]

### 2.2. С линейным сопротивлением воздуха

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

---

## 3. Что нового в интерфейсе

- Тёмная «игровая» тема и неоновая траектория.
- Фон сцены с «землёй», визуальной целью и маркером текущей позиции.
- HUD-панель с ключевыми метриками (\(T\), \(H_{\max}\), \(L\), скорость).
- Цветовая индикация силы сопротивления воздуха.
- Фиксированный масштаб + ручные регуляторы для честного сравнения экспериментов.

---

## 4. Код виртуальной лабораторной работы (Wolfram Mathematica)

> Скопируйте код в Mathematica Notebook и выполните целиком.

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
  root = Quiet@Check[t /. FindRoot[yf[t] == 0, {t, 10^-4, tMax}], Missing["NoRoot"]];
  If[NumericQ[root] && 0 <= root <= tMax, root, tMax]
  ];

(* ===== Вспомогательные элементы стиля ===== *)
hudCell[label_, value_, col_: White] :=
 Framed[
  Column[{Style[label, 10, GrayLevel[.8], FontFamily -> "Source Sans Pro"],
    Style[value, 14, Bold, col, FontFamily -> "Source Sans Pro"]}, Spacings -> .2],
  Background -> RGBColor[0.10, 0.12, 0.16],
  FrameStyle -> Directive[GrayLevel[.3], Thickness[.002]],
  RoundingRadius -> 8,
  FrameMargins -> {{8, 8}, {5, 5}}
  ];

Manipulate[
 Module[
  {
   xF, yF, vxF, vyF,
   T, H, L,
   tNow, xt, yt, vxt, vyt, speed,
   tGrid,
   xTop, yTop, tPlotMax, vyTop,
   tRootMax,
   trajPlot, pointPlot, sceneOverlay,
   graphX, graphY, graphVy,
   rangePlot, rangeTable, anglesDeg,
   bgColor, glowColor, dragColor, targetX
   },

  bgColor = RGBColor[0.05, 0.07, 0.11];
  glowColor = RGBColor[0.25, 0.95, 1.0];
  dragColor = Blend[{RGBColor[0.2, 1, 0.5], RGBColor[1, 0.45, 0.2]}, Rescale[kDrag, {0, 0.6}]];

  tRootMax = Max[2, tSolveMax];
  {xF, yF, vxF, vyF} = solutionFunctions[v0, α, g, y0, kDrag, tRootMax];

  T = flightTimeFromY[yF, tRootMax];
  H = Quiet@Check[NMaxValue[{yF[t], 0 <= t <= T}, t], y0];
  L = Quiet@Check[xF[T], 0];

  tNow = Min[tAnim, T];
  xt = xF[tNow]; yt = yF[tNow];
  vxt = vxF[tNow]; vyt = vyF[tNow];
  speed = Sqrt[vxt^2 + vyt^2];

  xTop = Max[10, xWindow];
  yTop = Max[5, yWindow];
  tPlotMax = Max[1, tWindow];
  vyTop = Max[5, vyWindow];
  targetX = 0.85 xTop;

  tGrid = Subdivide[0, T, 280];

  trajPlot = ListLinePlot[
    Transpose[{xF /@ tGrid, yF /@ tGrid}],
    PlotStyle -> {Directive[glowColor, Thick]},
    Filling -> Axis,
    FillingStyle -> Directive[Opacity[.12, glowColor]],
    AxesLabel -> {Style["x, м", 12, White], Style["y, м", 12, White]},
    PlotLabel -> Style["PROJECTILE ARENA", 14, Bold, RGBColor[0.7, 0.95, 1]],
    GridLines -> {Range[0, xTop, xTop/10], Range[0, yTop, yTop/8]},
    GridLinesStyle -> Directive[Opacity[.15, White]],
    PlotRange -> {{0, xTop}, {0, yTop}},
    Background -> bgColor,
    AxesStyle -> Directive[White, 11],
    ImageSize -> 560
    ];

  sceneOverlay = Graphics[
    {
     (* земля *)
     Directive[RGBColor[0.15, 0.22, 0.16], Opacity[.9]],
     Rectangle[{0, -0.03 yTop}, {xTop, 0}],

     (* "цель" *)
     Directive[RGBColor[1, 0.75, 0.1], Thick],
     Circle[{targetX, 0.08 yTop}, 0.05 yTop],
     Circle[{targetX, 0.08 yTop}, 0.025 yTop],
     PointSize[0.018], Point[{targetX, 0.08 yTop}],

     (* текущая точка и вектор скорости *)
     Directive[RGBColor[1, 0.3, 0.4]], PointSize[0.022], Point[{xt, yt}],
     Directive[dragColor, Thick],
     Arrow[{{xt, yt}, {xt + arrowScale vxt, yt + arrowScale vyt}}],

     (* след-ореол вокруг точки *)
     Directive[Opacity[.25, RGBColor[1, 0.4, 0.6]]], Disk[{xt, yt}, 0.015 xTop]
     }
    ];

  graphX = Plot[
    xF[t], {t, 0, T},
    PlotStyle -> {Thick, RGBColor[0.95, 0.35, 0.35]},
    PlotLabel -> Style["x(t)", 12, Bold, White],
    AxesLabel -> {Style["t, c", White], Style["x, м", White]},
    PlotRange -> {{0, tPlotMax}, {0, xTop}},
    GridLines -> Automatic,
    GridLinesStyle -> Directive[Opacity[.14, White]],
    Background -> bgColor,
    AxesStyle -> White,
    ImageSize -> 250
    ];

  graphY = Plot[
    yF[t], {t, 0, T},
    PlotStyle -> {Thick, RGBColor[0.3, 0.8, 1.0]},
    PlotLabel -> Style["y(t)", 12, Bold, White],
    AxesLabel -> {Style["t, c", White], Style["y, м", White]},
    PlotRange -> {{0, tPlotMax}, {0, yTop}},
    GridLines -> Automatic,
    GridLinesStyle -> Directive[Opacity[.14, White]],
    Background -> bgColor,
    AxesStyle -> White,
    ImageSize -> 250
    ];

  graphVy = Plot[
    vyF[t], {t, 0, T},
    PlotStyle -> {Thick, dragColor},
    PlotLabel -> Style["vy(t)", 12, Bold, White],
    AxesLabel -> {Style["t, c", White], Style["vy, м/с", White]},
    PlotRange -> {{0, tPlotMax}, {-vyTop, vyTop}},
    GridLines -> Automatic,
    GridLinesStyle -> Directive[Opacity[.14, White]],
    Background -> bgColor,
    AxesStyle -> White,
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
    PlotStyle -> {Thick, RGBColor[1.0, 0.65, 0.15]},
    PlotMarkers -> Automatic,
    AxesLabel -> {Style["Угол, град", White], Style["Дальность, м", White]},
    PlotLabel -> Style["Range vs Angle", 12, Bold, White],
    GridLines -> Automatic,
    GridLinesStyle -> Directive[Opacity[.14, White]],
    PlotRange -> {{0, 90}, {0, xTop}},
    Background -> bgColor,
    AxesStyle -> White,
    ImageSize -> 560,
    Epilog -> {
      RGBColor[1, 0.3, 0.4], PointSize[0.02], Point[{α/Degree, N@L}],
      White,
      Text[Style["Текущая конфигурация", 10, Bold], {Min[88, α/Degree + 7], Min[0.9 xTop, N@L + 0.06 xTop]}]
      }
    ];

  Framed[
   Column[
   {
    Style["🎮 Виртуальная лаборатория: Projectile Arena (с сопротивлением воздуха)", 16, Bold, White],

    Row[
     {
      hudCell["Коэффициент сопротивления k", Row[{NumberForm[kDrag, {4, 3}], " 1/с"}], dragColor],
      Spacer[8],
      hudCell["Время полёта T", Row[{NumberForm[T, {6, 3}], " c"}], RGBColor[0.6, 0.9, 1]],
      Spacer[8],
      hudCell["Макс. высота Hmax", Row[{NumberForm[H, {6, 3}], " м"}], RGBColor[0.5, 1, 0.75]],
      Spacer[8],
      hudCell["Дальность L", Row[{NumberForm[L, {6, 3}], " м"}], RGBColor[1, 0.82, 0.35]],
      Spacer[8],
      hudCell["Скорость |v|", Row[{NumberForm[speed, {6, 2}], " м/с"}], RGBColor[1, 0.55, 0.55]]
      }
     ],

    Show[trajPlot, sceneOverlay],
    GraphicsRow[{graphX, graphY, graphVy}, Spacings -> 10],
    rangePlot
    },
   Spacings -> 1.2
   ],
   Background -> bgColor,
   FrameStyle -> None,
   FrameMargins -> 8
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

## 5. Как работать

1. Запустите модуль и убедитесь, что интерфейс отображается в тёмной теме.
2. Для классического случая поставьте `k = 0`.
3. Для «игрового» сравнения увеличивайте `k` и наблюдайте изменение траектории/скорости/дальности.
4. Используйте фиксированный масштаб, чтобы визуально честно сравнивать разные запуски.

---

## 6. Таблица для отчёта

| № опыта | \(v_0\), м/с | \(\alpha\), град | \(y_0\), м | \(g\), м/с² | \(k\), 1/с | \(T\), с | \(H_{\max}\), м | \(L\), м |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 20 | 45 | 0 | 9.81 | 0.00 |  |  |  |
| 2 | 20 | 45 | 0 | 9.81 | 0.05 |  |  |  |
| 3 | 20 | 45 | 0 | 9.81 | 0.10 |  |  |  |
| 4 | 20 | 45 | 0 | 9.81 | 0.20 |  |  |  |
| 5 | 20 | 45 | 0 | 9.81 | 0.30 |  |  |  |

---

## 7. Вывод (пример)

Графически улучшенный интерфейс сделал лабораторную более наглядной: тёмная игровая сцена, HUD и цветовые подсказки позволяют быстрее понимать влияние сопротивления воздуха на траекторию и кинематические параметры.
