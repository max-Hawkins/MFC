import enum
import json
import typing
import dataclasses

import mfc.common

from dataclasses import field, fields

# === (RE)DEFINE FORTRAN CONSTANTS === #

DFLT_REAL = -1e6
DFLT_INT  = -100

# === DEFINE ALL ENUMERATIONS === #


class PatchGeometry(enum.Enum):
    D1_LINE_SEGMENT =  1
    D2_CIRCLE       =  2
    D2_RECTANGLE    =  3
    D2_SWEEP_LINE   =  4
    D2_ELLIPSE      =  5
    D2_VORTEX       =  6
    D2_ANALYTICAL   =  7
    D3_SPHERE       =  8
    D3_CUBOID       =  9
    D3_CYLINDER     = 10
    D3_SWEEP_PLANE  = 11
    D3_ELLIPSOID    = 12
    D3_ANALYTICAL   = 13


class FluxLimiter(enum.Enum):
    MINMOD     = 1
    MC         = 2
    OSPRE      = 3
    SUPERBEE   = 4
    SWEBY      = 5
    VAN_ALBADA = 6
    VAN_LEER   = 7


class MulticomponentModel(enum.Enum):
    GAMMA_PI_INF = 1
    EQUATION_5   = 2
    EQUATION_6   = 3


class BubbleModel(enum.Enum):
    GILMORE          = 1
    KELLER_MIKSIS    = 2
    RAYLEIGH_PLESSET = 3


class ThermalModel(enum.Enum):
    ADIABATIC  = 1
    ISOTHERMAL = 2
    TRANSFER   = 3


class BoundaryCondition(enum.Enum):
    PERIODIC                           =  -1
    REFLECTIVE                         =  -2
    GHOST_CELL_EXTRAPOLATION           =  -3
    RIEMANN_EXTRAPOLATION              =  -4
    SLIP_WALL                          =  -5
    NON_REFLECTING_SUBSONIC_BUFFER     =  -6
    NON_REFLECTING_SUBSONIC_INFLOW     =  -7
    NON_REFLECTING_SUBSONIC_OUTFLOW    =  -8
    FORCE_FREE_SUBSONIC_OUTFLOW        =  -9
    CONSTANT_PRESSURE_SUBSONIC_OUTFLOW = -10
    SUPERSONIC_INFLOW                  = -11
    SUPERSONIC_OUTFLOW                 = -12


class DatabaseFormat(enum.Enum):
    SILO_HDF5 = 1
    BINARY    = 2


class FloatingPrecision(enum.Enum):
    SINGLE = 1
    DOUBLE = 2


class RiemannSolver(enum.Enum):
    HLL   = 1
    HLLC  = 2
    EXACT = 3


class WaveSpeedEstimation(enum.Enum):
    DIRECT            = 1
    PRESSURE_VELOCITY = 2


class AverageStateEvaluation(enum.Enum):
    ROE_MEAN        = 1
    ARITHMETIC_MEAN = 2


class TimeStepper(enum.Enum):
    RUNGE_KUTTA_1 = 1
    RUNGE_KUTTA_2 = 2
    RUNGE_KUTTA_3 = 3
    RUNGE_KUTTA_4 = 4
    RUNGE_KUTTA_5 = 5


class WenoVariables(enum.Enum):
    CONSERVATIVE = 1
    PRIMITIVE    = 2


class AcousticWaveForm(enum.Enum):
    SINE     = 1
    GAUSSIAN = 2
    SQUARE   = 3


class AcousticSpacialSupport(enum.Enum):
    D1                   = 1
    D2_FINITE_WIDTH      = 2
    D3_FINITE_LINE_PATCH = 3


class BubbleDistribution(enum.Enum):
    BINORMAL = 1
    LOGNORMAL_NORMAL = 2



# === DEFINE UTILITY FUNCTIONS === #


def gen_f90_array_constructor(f90_type: str, arr: list) -> str:
    if len(arr) == 0:
        return f"[{f90_type} ::]"
    
    ss: typing.List[str] = []

    for e in arr:
        if callable(getattr(e, "to_f90", None)):
            ss.append(e.to_f90())
        elif f90_type == "logical":
            ss.append(f".{e}.")
        else:
            ss.append(f"{e}")

    return f"(/ {f', '.join(ss)} /)"


# === DEFINE UTILITY CLASSES === #


@dataclasses.dataclass
class Point:
    x: float = dataclasses.field(default_factory=lambda: 0)
    y: float = dataclasses.field(default_factory=lambda: 0)
    z: float = dataclasses.field(default_factory=lambda: 0)


class Vector(Point):
    pass


# ===    DEFINE CLASSES    === #
# === COMPUTATIONAL DOMAIN === #


@dataclasses.dataclass
class AxisDomain:
    begin: float = field(default_factory=lambda: 0)
    end:   float = field(default_factory=lambda: 0)

    def to_f90(self) -> str:
        return f"bounds_info({float(self.begin)}, {float(self.end)})"


@dataclasses.dataclass
class AxisStretch(AxisDomain):
    stretch:   bool   = field(default_factory=lambda: False)
    rate:      float  = field(default_factory=lambda: 0.0)
    loops:     int    = field(default_factory=lambda: 1)
    begin_pos: float  = field(default_factory=lambda: DFLT_REAL)
    begin_neg: float  = field(default_factory=lambda: DFLT_REAL)


@dataclasses.dataclass
class Stretch:
    x: AxisStretch = field(default_factory=lambda: AxisStretch())
    y: AxisStretch = field(default_factory=lambda: AxisStretch())
    z: AxisStretch = field(default_factory=lambda: AxisStretch())


@dataclasses.dataclass
class Cells:
    x: int
    y: int = field(default_factory=lambda: 0)
    z: int = field(default_factory=lambda: 0)


@dataclasses.dataclass
class SpacialDomain:
    x: AxisDomain
    y: AxisDomain = field(default_factory=lambda: AxisDomain(begin=DFLT_REAL, end=DFLT_REAL))
    z: AxisDomain = field(default_factory=lambda: AxisDomain(begin=DFLT_REAL, end=DFLT_REAL))


@dataclasses.dataclass
class Time:
    end:   int
    save:  int
    dt:    float
    begin: int   = field(default_factory=lambda: 0)


@dataclasses.dataclass
class ComputationalDomain:
    cells:        Cells
    domain:       SpacialDomain
    time:         Time
    cyl_coord:    bool    = field(default_factory=lambda: False)
    stretch:      Stretch = field(default_factory=lambda: Stretch())


# ===        FLUIDS        === #


@dataclasses.dataclass
class Fluid:
    gamma:   float              = field(default_factory=lambda: DFLT_REAL)
    pi_inf:  float              = field(default_factory=lambda: DFLT_REAL)
    Re:      typing.List[float] = field(default_factory=lambda: [DFLT_REAL, DFLT_REAL])
    mul0:    float              = field(default_factory=lambda: DFLT_REAL)
    ss:      float              = field(default_factory=lambda: DFLT_REAL)
    pv:      float              = field(default_factory=lambda: DFLT_REAL)
    gamma_v: float              = field(default_factory=lambda: DFLT_REAL)
    M_v:     float              = field(default_factory=lambda: DFLT_REAL)
    mu_v:    float              = field(default_factory=lambda: DFLT_REAL)
    k_v:     float              = field(default_factory=lambda: DFLT_REAL)
    G:       float              = field(default_factory=lambda: DFLT_REAL)

    def to_f90(self) -> str:
        members = []
        for field in fields(self):
            val = getattr(self, field.name)

            if isinstance(val, list):
                py_type = typing.get_args(field.type)[0]

                if py_type == float:
                    f90_type = "real(kind(0d0))"

                members.append(gen_f90_array_constructor(f90_type, list(map(str, val))))
            else:
                members.append(str(val))
            
        return f"physical_parameters({', '.join(members)})"


# === SIMULATION ALGORITHM === #


@dataclasses.dataclass
class WenoParameters:
    order:     int
    variables: WenoVariables
    mapped:    bool  = field(default_factory=lambda: False)
    mp:        bool  = field(default_factory=lambda: False)
    flat:      bool  = field(default_factory=lambda: True)
    epsilon:   float = field(default_factory=lambda: 1e-16)
    average:   bool  = field(default_factory=lambda: False)
    Re_flux:   bool  = field(default_factory=lambda: False)


@dataclasses.dataclass
class AxisBoundaryCondition:
    begin: BoundaryCondition = DFLT_INT
    end:   BoundaryCondition = DFLT_INT
    # Note: Using "DFLT_REAL" instead of "DFLT_INT" doesn't make sense, but it is
    #       what is used by the fortran code.


@dataclasses.dataclass
class BoundaryConditions:
    x: AxisBoundaryCondition
    y: AxisBoundaryCondition = field(default_factory=lambda: AxisBoundaryCondition())
    z: AxisBoundaryCondition = field(default_factory=lambda: AxisBoundaryCondition())


@dataclasses.dataclass
class SimulationAlgorithm:
    model:              MulticomponentModel
    weno:               WenoParameters
    boundary:           BoundaryConditions
    time_stepper:       TimeStepper
    wave_speeds:        WaveSpeedEstimation
    avg_state:          AverageStateEvaluation
    riemann_solver:     RiemannSolver
    riemann_flat:       bool        = field(default_factory=lambda: True)
    char_decomp:        bool        = field(default_factory=lambda: False)
    commute_err:        bool        = field(default_factory=lambda: False)
    split_err:          bool        = field(default_factory=lambda: False)
    mixture_err:        bool        = field(default_factory=lambda: False)
    mpp_lim:            bool        = field(default_factory=lambda: False)
    adv_alphan:         bool        = field(default_factory=lambda: False)
    hypoelasticity:     bool        = field(default_factory=lambda: False)
    null_weights:       bool        = field(default_factory=lambda: False)
    alt_crv:            bool        = field(default_factory=lambda: False)
    alt_soundspeed:     bool        = field(default_factory=lambda: False)
    regularization:     bool        = field(default_factory=lambda: False)
    reg_eps:            float       = field(default_factory=lambda: DFLT_REAL)
    tvd_riemann_flux:   bool        = field(default_factory=lambda: False)
    tvd_rhs_flux:       bool        = field(default_factory=lambda: False)
    tvd_wave_speeds:    bool        = field(default_factory=lambda: False)
    flux_lim:           FluxLimiter = field(default_factory=lambda: DFLT_INT)
    We_riemann_flux:    bool        = field(default_factory=lambda: False)
    We_rhs_flux:        bool        = field(default_factory=lambda: False)
    We_src:             bool        = field(default_factory=lambda: False)
    We_wave_speeds:     bool        = field(default_factory=lambda: False)
    lsq_deriv:          bool        = field(default_factory=lambda: False)
    hypoelasticity:     bool        = field(default_factory=lambda: False)
    perturb_flow:       bool        = field(default_factory=lambda: False)
    perturb_flow_fluid: int         = field(default_factory=lambda: DFLT_INT)
    perturb_sph:        bool        = field(default_factory=lambda: False)
    perturb_sph_fluid:  int         = field(default_factory=lambda: DFLT_INT)
    fluid_rho:          float       = field(default_factory=lambda: DFLT_REAL)
    rhoref:             float       = field(default_factory=lambda: DFLT_REAL) 
    pref:               float       = field(default_factory=lambda: DFLT_REAL)



# ===   PATCH PARAMETERS   === #



@dataclasses.dataclass
class Patch:
    geometry:        PatchGeometry      = field(default_factory=lambda: DFLT_INT)
    centroid:        Point              = field(default_factory=lambda: Vector(x=DFLT_REAL,y=DFLT_REAL,z=DFLT_REAL))
    length:          Vector             = field(default_factory=lambda: Vector(x=DFLT_REAL,y=DFLT_REAL,z=DFLT_REAL))
    radius:          float              = field(default_factory=lambda: DFLT_REAL)
    radii:           Vector             = field(default_factory=lambda: Vector(x=DFLT_REAL,y=DFLT_REAL,z=DFLT_REAL))
    epsilon:         float              = field(default_factory=lambda: DFLT_REAL)
    beta:            float              = field(default_factory=lambda: DFLT_REAL)
    normal:          Vector             = field(default_factory=lambda: Vector(x=DFLT_REAL,y=DFLT_REAL,z=DFLT_REAL))
    alter:           typing.List[bool]  = field(default_factory=lambda: [])
    smoothen:        bool               = field(default_factory=lambda: False)
    smooth_patch_id: int                = field(default_factory=lambda: DFLT_INT)
    smooth_coeff:    float              = field(default_factory=lambda: DFLT_REAL)
    alpha_rho:       typing.List[float] = field(default_factory=lambda: [])
    rho:             float              = field(default_factory=lambda: DFLT_REAL)
    velocity:        Vector             = field(default_factory=lambda: Vector(x=DFLT_REAL,y=DFLT_REAL,z=DFLT_REAL))
    pressure:        float              = field(default_factory=lambda: DFLT_REAL)
    alpha:           typing.List[float] = field(default_factory=lambda: [])
    gamma:           float              = field(default_factory=lambda: DFLT_REAL)
    pi_inf:          float              = field(default_factory=lambda: DFLT_REAL)
    r0:              float              = field(default_factory=lambda: DFLT_REAL)
    v0:              float              = field(default_factory=lambda: DFLT_REAL)
    p0:              float              = field(default_factory=lambda: DFLT_REAL)
    m0:              float              = field(default_factory=lambda: DFLT_REAL)


    def to_f90(self) -> str:
        members = []
        for field in fields(self):
            val = getattr(self, field.name)

            if field.name in ["radii", "normal", "velocity"]:
                members.append(f"(/ {float(val.x)}, {float(val.y)}, {float(val.z)} /)")
            elif isinstance(val, Point) or isinstance(val, Vector):
                members.append(f"{float(val.x)}, {float(val.y)}, {float(val.z)}")
            elif isinstance(val, enum.Enum):
                members.append(str(val.value))
            elif isinstance(val, list):
                py_type = typing.get_args(field.type)[0]

                if py_type == bool:
                    f90_type = "logical"
                elif py_type == float:
                    f90_type = "real(kind(0d0))"

                members.append(gen_f90_array_constructor(f90_type, val))
            elif isinstance(val, bool):
                members.append(f".{val}.")
            else:
                members.append(str(val))
            
        return f"ic_patch_parameters({', '.join(members)})"


# ===  DATABASE STRUCTURE  === #


@dataclasses.dataclass
class AxisMinMax:
    min: float = field(default_factory=lambda: DFLT_REAL)
    max: float = field(default_factory=lambda: DFLT_REAL)


@dataclasses.dataclass
class Integral:
    x: AxisMinMax = field(default_factory=lambda: AxisMinMax())
    y: AxisMinMax = field(default_factory=lambda: AxisMinMax())
    z: AxisMinMax = field(default_factory=lambda: AxisMinMax())

    def to_f90(self) -> str:
        return f"integral_parameters({float(self.x.min)}, {float(self.x.max)}, {float(self.y.min)}, {float(self.y.max)}, {float(self.z.min)}, {float(self.z.max)})"


@dataclasses.dataclass
class Probe(Vector):
    def to_f90(self) -> str:
        return f"probe_parameters({float(self.x)}, {float(self.y)}, {float(self.z)})"


@dataclasses.dataclass
class DatabaseWrite:
    alpha_rho:    bool = field(default_factory=lambda: False)
    rho:          bool = field(default_factory=lambda: False)
    mom:          bool = field(default_factory=lambda: False)
    velocity:     bool = field(default_factory=lambda: False)
    flux:         bool = field(default_factory=lambda: False)
    E:            bool = field(default_factory=lambda: False)
    pressure:     bool = field(default_factory=lambda: False)
    alpha:        bool = field(default_factory=lambda: False)
    gamma:        bool = field(default_factory=lambda: False)
    heat_ratio:   bool = field(default_factory=lambda: False)
    pi_inf:       bool = field(default_factory=lambda: False)
    pressure_inf: bool = field(default_factory=lambda: False)
    prim_vars:    bool = field(default_factory=lambda: False)
    cons_vars:    bool = field(default_factory=lambda: False)
    c:            bool = field(default_factory=lambda: False)
    omega:        bool = field(default_factory=lambda: False)
    schlieren:    bool = field(default_factory=lambda: False)
    probe:        bool = field(default_factory=lambda: False)
    integral:     bool = field(default_factory=lambda: False)
    #TODO: Some (i) params


@dataclasses.dataclass
class DatabseStructure:
    precision:      FloatingPrecision = field(default_factory=lambda: FloatingPrecision.DOUBLE)
    fd_order:       int               = field(default_factory=lambda: DFLT_INT)
    write:          DatabaseWrite     = field(default_factory=lambda: DatabaseWrite())
    alt_soundspeed: bool              = field(default_factory=lambda: False)
    parallel_io:    bool              = field(default_factory=lambda: False)
    coarsen_silo:   bool              = field(default_factory=lambda: False)
    format:         DatabaseFormat    = field(default_factory=lambda: DatabaseFormat.SILO_HDF5)
    probes:         typing.List[Probe]    = field(default_factory=lambda: [])
    integrals:      typing.List[Integral] = field(default_factory=lambda: [])


# ===       BUBBLES       === #


@dataclasses.dataclass
class Bubbles:
    bubbles:      bool               = field(default_factory=lambda: False)
    model:        BubbleModel        = field(default_factory=lambda: BubbleModel.GILMORE)
    polytropic:   bool               = field(default_factory=lambda: True)
    thermal:      ThermalModel       = field(default_factory=lambda: DFLT_INT)
    R0ref:        float              = field(default_factory=lambda: DFLT_REAL)
    number:       int                = field(default_factory=lambda: DFLT_INT)
    cavitation:   float              = field(default_factory=lambda: DFLT_REAL)
    weber:        float              = field(default_factory=lambda: DFLT_REAL)
    Re_inv:       float              = field(default_factory=lambda: DFLT_REAL)
    mu_10:        float              = field(default_factory=lambda: DFLT_REAL)
    ss:           float              = field(default_factory=lambda: DFLT_REAL)
    pv:           float              = field(default_factory=lambda: DFLT_REAL)
    gamma_v:      float              = field(default_factory=lambda: DFLT_REAL)
    M_v:          float              = field(default_factory=lambda: DFLT_REAL)
    mu_v:         float              = field(default_factory=lambda: DFLT_REAL)
    k_v:          float              = field(default_factory=lambda: DFLT_REAL)
    qbmm:         bool               = field(default_factory=lambda: False)
    polydisperse: bool               = field(default_factory=lambda: False)
    nnode:        int                = field(default_factory=lambda: 1)
    sigR:         float              = field(default_factory=lambda: DFLT_REAL)
    sigV:         float              = field(default_factory=lambda: DFLT_REAL)
    rhoRV:        float              = field(default_factory=lambda: 0)
    poly_sigma:   float              = field(default_factory=lambda: DFLT_REAL)
    distribution: BubbleDistribution = field(default_factory=lambda: DFLT_INT)
    R0_type:      int                = field(default_factory=lambda: DFLT_INT)

# === LOGISTICS === #


@dataclasses.dataclass
class Logistics:
    case_dir:      str  = field(default_factory=lambda: '.')
    run_time_info: bool = field(default_factory=lambda: False)
    cu_mpi:        bool = field(default_factory=lambda: False)
    cu_tensor:     bool = field(default_factory=lambda: False)
    debug:         bool = field(default_factory=lambda: False)
    old_grid:      bool = field(default_factory=lambda: False)
    old_ic:        bool = field(default_factory=lambda: False)
    t_step_old:    int  = field(default_factory=lambda: DFLT_INT)


# === === #


@dataclasses.dataclass
class Monopole:    
    location:  Point                  = field(default_factory=lambda: Point(x=DFLT_REAL,y=DFLT_REAL,z=DFLT_REAL))
    magnitude: float                  = field(default_factory=lambda: DFLT_REAL)
    length:    float                  = field(default_factory=lambda: DFLT_REAL)
    npulse:    float                  = field(default_factory=lambda: 1.0)
    direction: float                  = field(default_factory=lambda: 1.0)
    delay:     float                  = field(default_factory=lambda: DFLT_REAL)
    pulse:     AcousticWaveForm       = field(default_factory=lambda: AcousticWaveForm.SINE)
    support:   AcousticSpacialSupport = field(default_factory=lambda: AcousticSpacialSupport.D1)


    def to_f90(self) -> str:
        members = []
        for field in fields(self):
            val = getattr(self, field.name)

            if isinstance(val, Point) or isinstance(val, Vector):
                members.append(f"(/ {float(val.x)}, {float(val.y)}, {float(val.z)} /)")
            elif isinstance(val, enum.Enum):
                members.append(f"{val.value}")
            else:
                members.append(str(val))
            
        return f"mono_parameters({', '.join(members)})"


@dataclasses.dataclass
class AcousticParameters:
    monopole:  bool = field(default_factory=lambda: False)
    monopoles: typing.List[Monopole] = field(default_factory=lambda: [])


# === DEFINE A CASE === #


@dataclasses.dataclass
class Case:
    logistics: Logistics
    domain:    ComputationalDomain
    fluids:    typing.List[Fluid]
    patches:   typing.List[Patch]
    bubbles:   Bubbles
    database:  DatabseStructure
    algorithm: SimulationAlgorithm
    acoustic:  AcousticParameters


    def __repr__(self) -> str:
        return json.dumps(self.to_json(), indent=4)
    
    def set(self, path: str, value: typing.Any):
        expr = f"self.{path}={value}"

        # We may need to "allocate" another fluid
        match = "self.fluids["
        if expr.startswith(match):
            fluid_idx = int(expr[len(match):].split(']')[0])

            if fluid_idx + 1 > len(self.fluids):
                self.fluids.append(Fluid())

        exec(expr)

    def to_json(self) -> dict:
        def handle_enums(data):
            def convert_value(obj):
                if isinstance(obj, enum.Enum):
                    return obj.value

                return obj

            return dict((key, convert_value(value)) for key, value in data)

        # We autogenerate here some parameters to make the Fortran file cleaner.
        # Generating these parameters using Fypp would lead to a lot fo code
        # duplication as well as very long and unreadable python one-liners.
        # It also has the benefit of consolidating all input parameter logic
        # in one place.

        for id, patch in enumerate(self.patches):
            patch.smooth_patch_id = id + 1

        # Compute grid_geometry
        grid_geometry = None
        if self.domain.cyl_coord is False:
            grid_geometry = 1
        elif self.domain.cyl_coord and self.domain.cells.z == 0:
            grid_geometry = 2
        else:
            grid_geometry = 3

        # num_fluids
        if self.algorithm.model == 1 and len(self.fluids) != 1:
            raise mfc.common.MFCException("Invalid combination.")

        num_fluids       = len(self.fluids)
        num_fluids_alloc = len(self.fluids) + 1 # +1 is a workaround for the bellow:

        # Workaround for https://github.com/henryleberre/MFC/blob/f6620bba0f248b729491dd3b0b1dd070ddd63c2d/src/common/m_global_parameters.fpp#L511-L515
        # if num_fluids == 1 and self.bubbles.bubbles and self.algorithm.model == MulticomponentModel.EQUATION_5:
        #     num_fluids_alloc += 1

        # patches & alter_patches
        patches = self.patches.copy()
        for patch in patches:
            patch.alter     = [True] + patch.alter     + [False]    *(len(self.patches)-len(patch.alter))
            patch.alpha     =          patch.alpha     + [DFLT_REAL]*(num_fluids-len(patch.alpha))
            patch.alpha_rho =          patch.alpha_rho + [DFLT_REAL]*(num_fluids-len(patch.alpha_rho))

        return {**dataclasses.asdict(self, dict_factory=handle_enums), **{
            "autogen": {
                "num_fluids":       num_fluids,
                "num_fluids_alloc": num_fluids_alloc,
                "grid_geometry":    grid_geometry,
                "x_domain":         self.domain.domain.x.to_f90(),
                "y_domain":         self.domain.domain.y.to_f90(),
                "z_domain":         self.domain.domain.z.to_f90(),
                "patch_icpp":       gen_f90_array_constructor("ic_patch_parameters", patches),
                "fluid_pp":         gen_f90_array_constructor("physical_parameters", self.fluids + [Fluid()]),
                "mono":             gen_f90_array_constructor("mono_parameters",     self.acoustic.monopoles),
                "probe":            gen_f90_array_constructor("probe_parameters",    self.database.probes),
                "integral":         gen_f90_array_constructor("integral_parameters", self.database.integrals),
            }
        }}
