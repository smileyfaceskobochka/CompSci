import React, { useEffect, useState } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from "@/components/ui/table";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Dialog, DialogContent, DialogDescription, DialogFooter, DialogHeader, DialogTitle, DialogTrigger } from "@/components/ui/dialog";
import { Trophy, Users, ShieldAlert, Sparkles, Plus, Trash2, Edit2, Play, GitCompare, RefreshCw, Landmark, Zap, Compass, Check, Calendar, MapPin, Gauge } from "lucide-react";
import "./index.css";

// --- Types ---
interface Driver {
  id: number;
  teamId: number;
  firstName: string;
  lastName: string;
  racingNumber: number;
  points: number;
}

interface Team {
  id: number;
  teamName: string;
  principalName: string;
  headquarters: string;
  championshipPoints: number;
  raceWins: number;
  podiums: number;
  drivers: Driver[];
  team_color?: string;
  visible_color?: string;
  engine_status?: string;
  feeder_status_text?: string;
  battery_rating_text?: string;
  type: "f1" | "f2" | "fe";
  
  // F1 fields
  powerUnit?: string;
  budgetCapMln?: number;
  constructorPos?: number;
  
  // F2 fields
  chassisModel?: string;
  f1Graduates?: number;
  isFeederSeries?: boolean;
  
  // FE fields
  sustainabilityScore?: number;
  batteryCapacityKwh?: number;
  energyPartner?: string;
}



export function App() {
  const raceControlMessages = [
    "TRACK STATUS: ALL SECTORS GREEN // ALL TRANSPONDERS ACTIVE",
    "WEATHER ADVISORY: 20% RISK OF SHOWERS IN NEXT 10 MINUTES // TRACK TEMP STEADY",
    "RACE DIRECTION: SPEED LIMIT 80 KM/H ENFORCED IN PIT LANE",
    "TRACK STATUS: ALL SECTORS GREEN // TELEMETRY LINK ONLINE",
    "SAFETY DEPLOYMENT CHECK: YELLOW FLAGS CLEARED // PIT ENTRY OPEN",
    "FIA NOTE: CAR 44 TIME PENALTY RESOLVED (+5.000s)"
  ];
  const [tickerIndex, setTickerIndex] = useState(0);

  const [teams, setTeams] = useState<Team[]>([]);
  const [drivers, setDrivers] = useState<Driver[]>([]);
  const [dashboardStats, setDashboardStats] = useState({
    registeredTeamsCount: 0,
    activeDriversCount: 0,
    averagePoints: 0,
    leadingTeamName: "N/A",
    leadingTeamPoints: 0,
    leadingTeamWins: 0,
    leadingDriverName: "N/A",
    leadingDriverPoints: 0,
    leadingDriverNumber: 0
  });
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Active filter tab
  const [seriesFilter, setSeriesFilter] = useState<string>("all");
  const [hoveredCardId, setHoveredCardId] = useState<number | null>(null);

  // Modals state
  const [isTeamModalOpen, setIsTeamModalOpen] = useState(false);
  const [isDriverModalOpen, setIsDriverModalOpen] = useState(false);
  const [isCompareModalOpen, setIsCompareModalOpen] = useState(false);
  const [isSimulateModalOpen, setIsSimulateModalOpen] = useState(false);

  // Current editing items
  const [editingTeam, setEditingTeam] = useState<Team | null>(null);
  const [editingDriver, setEditingDriver] = useState<Driver | null>(null);
  const [simulatingTeam, setSimulatingTeam] = useState<Team | null>(null);
  const [simulatingDriverId, setSimulatingDriverId] = useState<string>("");

  // Comparison State
  const [compareTeamA, setCompareTeamA] = useState<string>("");
  const [compareTeamB, setCompareTeamB] = useState<string>("");
  const [compareResult, setCompareResult] = useState<any>(null);

  // Simulation State
  const [simPosition, setSimPosition] = useState<number>(1);

  // Global Simulation State
  const [isGlobalSimModalOpen, setIsGlobalSimModalOpen] = useState(false);
  const [simSeries, setSimSeries] = useState<"f1" | "f2" | "fe">("f1");
  const [simDrivers, setSimDrivers] = useState<Driver[]>([]);
  const [draggedIndex, setDraggedIndex] = useState<number | null>(null);

  // Form states - Team
  const [teamType, setTeamType] = useState<"f1" | "f2" | "fe">("f1");
  const [teamName, setTeamName] = useState("");
  const [principalName, setPrincipalName] = useState("");
  const [headquarters, setHeadquarters] = useState("");
  const [teamColor, setTeamColor] = useState("#FFFFFF");
  const [championshipPoints, setChampionshipPoints] = useState(0);
  const [raceWins, setRaceWins] = useState(0);
  const [podiums, setPodiums] = useState(0);
  // Type-specific inputs
  const [powerUnit, setPowerUnit] = useState("Ferrari");
  const [budgetCapMln, setBudgetCapMln] = useState(135);
  const [constructorPos, setConstructorPos] = useState(1);
  const [chassisModel, setChassisModel] = useState("Dallara F2 2024");
  const [f1Graduates, setF1Graduates] = useState(0);
  const [isFeederSeries, setIsFeederSeries] = useState(true);
  const [sustainabilityScore, setSustainabilityScore] = useState(80);
  const [batteryCapacityKwh, setBatteryCapacityKwh] = useState(38);
  const [energyPartner, setEnergyPartner] = useState("DHL");

  // Form states - Driver
  const [driverFirstName, setDriverFirstName] = useState("");
  const [driverLastName, setDriverLastName] = useState("");
  const [driverNumber, setDriverNumber] = useState(0);
  const [driverPoints, setDriverPoints] = useState(0);
  const [driverTeamId, setDriverTeamId] = useState<string>("");

  const fetchStats = async (series: string) => {
    try {
      const res = await fetch(`/api/stats?series=${series}`);
      if (!res.ok) throw new Error("Failed to fetch stats");
      const data = await res.json();
      setDashboardStats(data);
    } catch (err) {
      console.error("Error loading stats:", err);
    }
  };

  useEffect(() => {
    fetchData();
    const interval = setInterval(() => {
      setTickerIndex((prev) => (prev + 1) % raceControlMessages.length);
    }, 5000);
    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    fetchStats(seriesFilter);
  }, [seriesFilter, teams, drivers]);

  const fetchData = async () => {
    setIsLoading(true);
    setError(null);
    try {
      const teamsRes = await fetch("/api/teams");
      if (!teamsRes.ok) throw new Error("Failed to fetch teams");
      const teamsData = await teamsRes.json();
      setTeams(teamsData);

      const driversRes = await fetch("/api/drivers");
      if (!driversRes.ok) throw new Error("Failed to fetch drivers");
      const driversData = await driversRes.json();
      setDrivers(driversData);

      await fetchStats(seriesFilter);
    } catch (err: any) {
      console.error(err);
      setError(err.message || "An error occurred while loading data");
    } finally {
      setIsLoading(false);
    }
  };

  const handleOpenTeamAdd = () => {
    setEditingTeam(null);
    setTeamType("f1");
    setTeamName("");
    setPrincipalName("");
    setHeadquarters("");
    setTeamColor("#e2e8f0");
    setChampionshipPoints(0);
    setRaceWins(0);
    setPodiums(0);
    setPowerUnit("Mercedes");
    setBudgetCapMln(135.0);
    setConstructorPos(1);
    setChassisModel("Dallara F2 2024");
    setF1Graduates(0);
    setIsFeederSeries(true);
    setSustainabilityScore(80);
    setBatteryCapacityKwh(38.0);
    setEnergyPartner("DHL");
    setIsTeamModalOpen(true);
  };

  const handleOpenTeamEdit = (team: Team) => {
    setEditingTeam(team);
    setTeamType(team.type);
    setTeamName(team.teamName);
    setPrincipalName(team.principalName);
    setHeadquarters(team.headquarters);
    setTeamColor(team.team_color || "#ffffff");
    setChampionshipPoints(team.championshipPoints);
    setRaceWins(team.raceWins);
    setPodiums(team.podiums);
    setPowerUnit(team.powerUnit || "");
    setBudgetCapMln(team.budgetCapMln || 135.0);
    setConstructorPos(team.constructorPos || 1);
    setChassisModel(team.chassisModel || "");
    setF1Graduates(team.f1Graduates || 0);
    setIsFeederSeries(team.isFeederSeries !== false);
    setSustainabilityScore(team.sustainabilityScore || 50);
    setBatteryCapacityKwh(team.batteryCapacityKwh || 38.0);
    setEnergyPartner(team.energyPartner || "");
    setIsTeamModalOpen(true);
  };

  const handleSaveTeam = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      const payload: any = {
        type: teamType,
        teamName,
        principalName,
        headquarters,
        team_color: teamColor,
        championshipPoints,
        raceWins,
        podiums,
      };

      if (teamType === "f1") {
        payload.powerUnit = powerUnit;
        payload.budgetCapMln = Number(budgetCapMln);
        payload.constructorPos = Number(constructorPos);
      } else if (teamType === "f2") {
        payload.chassisModel = chassisModel;
        payload.f1Graduates = Number(f1Graduates);
        payload.isFeederSeries = isFeederSeries;
      } else if (teamType === "fe") {
        payload.sustainabilityScore = Number(sustainabilityScore);
        payload.batteryCapacityKwh = Number(batteryCapacityKwh);
        payload.energyPartner = energyPartner;
      }

      let res;
      if (editingTeam) {
        res = await fetch(`/api/teams/${editingTeam.id}`, {
          method: "PUT",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        });
      } else {
        res = await fetch("/api/teams", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        });
      }

      if (!res.ok) throw new Error("Failed to save team");
      setIsTeamModalOpen(false);
      fetchData();
    } catch (err: any) {
      alert(err.message || "Error saving team");
    }
  };

  const handleDeleteTeam = async (id: number) => {
    if (!confirm("Are you sure you want to delete this team? All associated drivers will also be deleted.")) return;
    try {
      const res = await fetch(`/api/teams/${id}`, { method: "DELETE" });
      if (!res.ok) throw new Error("Failed to delete team");
      fetchData();
    } catch (err: any) {
      alert(err.message || "Error deleting team");
    }
  };

  const handleOpenDriverAdd = () => {
    setEditingDriver(null);
    setDriverFirstName("");
    setDriverLastName("");
    setDriverNumber(0);
    setDriverPoints(0);
    setDriverTeamId(teams[0]?.id?.toString() || "");
    setIsDriverModalOpen(true);
  };

  const handleOpenDriverEdit = (driver: Driver) => {
    setEditingDriver(driver);
    setDriverFirstName(driver.firstName);
    setDriverLastName(driver.lastName);
    setDriverNumber(driver.racingNumber);
    setDriverPoints(driver.points);
    setDriverTeamId(driver.teamId.toString());
    setIsDriverModalOpen(true);
  };

  const handleSaveDriver = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      const payload = {
        firstName: driverFirstName,
        lastName: driverLastName,
        racingNumber: Number(driverNumber),
        points: Number(driverPoints),
        teamId: Number(driverTeamId),
      };

      let res;
      if (editingDriver) {
        res = await fetch(`/api/drivers/${editingDriver.id}`, {
          method: "PUT",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        });
      } else {
        res = await fetch("/api/drivers", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        });
      }

      if (!res.ok) throw new Error("Failed to save driver");
      setIsDriverModalOpen(false);
      fetchData();
    } catch (err: any) {
      alert(err.message || "Error saving driver");
    }
  };

  const handleDeleteDriver = async (id: number) => {
    if (!confirm("Are you sure you want to delete this driver?")) return;
    try {
      const res = await fetch(`/api/drivers/${id}`, { method: "DELETE" });
      if (!res.ok) throw new Error("Failed to delete driver");
      fetchData();
    } catch (err: any) {
      alert(err.message || "Error deleting driver");
    }
  };

  const handleOpenSimulate = (team: Team) => {
    setSimulatingTeam(team);
    setSimPosition(1);
    setSimulatingDriverId(team.drivers[0]?.id?.toString() || "");
    setIsSimulateModalOpen(true);
  };

  const handleRunSimulation = async () => {
    if (!simulatingTeam) return;
    if (!simulatingDriverId) {
      alert("Please sign a driver to this team before simulating a race.");
      return;
    }
    try {
      const url = `/api/teams/${simulatingTeam.id}/simulate-race` +
                  `?position=${simPosition}&driverId=${simulatingDriverId}`;
      const res = await fetch(url, {
        method: "POST"
      });
      if (!res.ok) throw new Error("Simulation failed");
      const data = await res.json();
      setIsSimulateModalOpen(false);
      fetchData();
    } catch (err: any) {
      alert(err.message || "Error during simulation");
    }
  };

  const handleOpenCompare = () => {
    setCompareTeamA(teams[0]?.id?.toString() || "");
    setCompareTeamB(teams[1]?.id?.toString() || "");
    setCompareResult(null);
    setIsCompareModalOpen(true);
  };

  const handleRunComparison = async () => {
    try {
      const url = `/api/teams/compare` +
                  `?teamIdA=${compareTeamA}&teamIdB=${compareTeamB}`;
      const res = await fetch(url);
      if (!res.ok) throw new Error("Comparison failed");
      const data = await res.json();
      setCompareResult(data);
    } catch (err: any) {
      alert(err.message || "Error during comparison");
    }
  };

  useEffect(() => {
    if (isGlobalSimModalOpen) {
      const filtered = drivers.filter(d => {
        const t = teams.find(team => team.id === d.teamId);
        return t?.type === simSeries;
      });
      filtered.sort((a, b) => b.points - a.points);
      setSimDrivers(filtered);
    }
  }, [isGlobalSimModalOpen, simSeries, drivers, teams]);

  const handleOpenGlobalSim = () => {
    if (seriesFilter === "f1" || seriesFilter === "f2" || seriesFilter === "fe") {
      setSimSeries(seriesFilter as "f1" | "f2" | "fe");
    } else {
      setSimSeries("f1");
    }
    setIsGlobalSimModalOpen(true);
  };

  const handleDragStart = (index: number) => {
    setDraggedIndex(index);
  };

  const handleDragOver = (e: React.DragEvent) => {
    e.preventDefault();
  };

  const handleDrop = (targetIndex: number) => {
    if (draggedIndex === null || draggedIndex === targetIndex) return;
    const list = [...simDrivers];
    const item = list[draggedIndex];
    list.splice(draggedIndex, 1);
    list.splice(targetIndex, 0, item);
    setSimDrivers(list);
    setDraggedIndex(null);
  };

  const handleRunGlobalSimulation = async () => {
    try {
      const payload = simDrivers.map((d, idx) => ({
        driverId: d.id,
        position: idx + 1
      }));

      const res = await fetch("/api/simulation/race", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload)
      });

      if (!res.ok) {
        throw new Error("Failed to submit global race simulation results.");
      }

      await fetchData(); // Refresh standings
      setIsGlobalSimModalOpen(false);
    } catch (err: any) {
      console.error(err);
      alert(err.message || "Simulation failed");
    }
  };

  const getSeriesDisplay = (type: string) => {
    if (type === "f1") return "Formula 1";
    if (type === "f2") return "Formula 2";
    return "Formula E";
  };

  const filteredTeams = seriesFilter === "all" 
    ? teams 
    : teams.filter(t => t.type === seriesFilter);

  const filteredDrivers = drivers.filter(d => {
    if (seriesFilter === "all") return true;
    const t = teams.find(team => team.id === d.teamId);
    return t?.type === seriesFilter;
  });

  return (
    <div className="min-h-screen bg-[#07080f] text-zinc-100 flex flex-col antialiased relative">
      {/* Premium Gradient Blobs */}
      <div className="absolute top-[-10%] left-[-10%] w-[550px] h-[550px] rounded-full bg-orange-600/10 blur-[130px] pointer-events-none z-0" />
      <div className="absolute bottom-[20%] right-[-10%] w-[650px] h-[650px] rounded-full bg-blue-600/5 blur-[160px] pointer-events-none z-0" />
      <div className="absolute top-[40%] left-[30%] w-[450px] h-[450px] rounded-full bg-violet-600/5 blur-[140px] pointer-events-none z-0" />
      <div className="absolute top-0 right-0 left-0 h-[1px] bg-gradient-to-r from-transparent via-white/10 to-transparent pointer-events-none" />

      {/* Header */}
      <header className="border-b border-white/5 bg-zinc-950/60 backdrop-blur-xl sticky top-0 z-50 transition-all duration-300">
        <div className="w-full max-w-7xl mx-auto px-8 py-5 flex items-center justify-between">
          <div className="flex items-center gap-4">

            <div>
              <h1 className="font-extrabold text-2xl tracking-tight bg-clip-text text-transparent bg-gradient-to-r from-white via-zinc-200 to-zinc-400">
                FORMULA RACING
              </h1>
              <p className="text-[9px] text-zinc-400 font-bold tracking-widest uppercase mt-0.5">
                Racing Management System
              </p>
            </div>
          </div>

          <div className="flex items-center gap-3">
            <Button
              variant="outline"
              size="sm"
              onClick={handleOpenCompare}
              className="border-white/10 bg-white/[0.02] hover:bg-white/[0.08] hover:text-white transition-all text-xs font-semibold text-zinc-300 rounded-xl"
            >
              <GitCompare className="mr-2 h-4 w-4 text-orange-500" />
              Compare Teams
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={handleOpenGlobalSim}
              className="border-white/10 bg-white/[0.02] hover:bg-white/[0.08] hover:text-white transition-all text-xs font-semibold text-zinc-300 rounded-xl"
            >
              <Play className="mr-2 h-4 w-4 text-emerald-500 animate-pulse" />
              Simulate GP
            </Button>
            <Button
              size="sm"
              variant="ghost"
              onClick={handleOpenTeamAdd}
              className="bg-gradient-to-r from-amber-500 via-orange-600 to-red-600 hover:from-amber-600 hover:to-red-700 text-white shadow-lg shadow-orange-600/20 text-xs font-bold px-4 rounded-xl border border-white/10 transition-all hover:scale-[1.02] hover:bg-transparent"
            >
              <Plus className="mr-1 h-4 w-4" />
              Add Team
            </Button>
            <Button
              size="icon"
              variant="ghost"
              onClick={fetchData}
              className="text-zinc-400 hover:text-white hover:bg-white/5 rounded-xl"
            >
              <RefreshCw className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </header>

      {/* Paddock Live Ticker */}
      <div className="bg-zinc-950 border-b border-white/5 py-2.5 overflow-hidden select-none relative z-40 bg-gradient-to-r from-zinc-950 via-zinc-900 to-zinc-950">
        <div className="w-full max-w-7xl mx-auto px-8 flex items-center gap-4">
          <span className="flex items-center gap-1.5 px-2.5 py-1 rounded bg-red-600 text-white font-mono font-black text-[9px] tracking-widest shrink-0">
            <span className="h-1.5 w-1.5 rounded-full bg-white animate-ping" />
            RACE CONTROL
          </span>
          <div className="flex-1 overflow-hidden">
            <div className="animate-fade-in font-mono text-[10px] text-zinc-300 font-bold tracking-wider truncate uppercase">
              {raceControlMessages[tickerIndex]}
            </div>
          </div>
          <div className="flex items-center gap-1 shrink-0">
            <Button
              size="icon"
              variant="ghost"
              className="h-6 w-6 text-zinc-400 hover:text-white rounded hover:bg-white/5"
              onClick={() => setTickerIndex((prev) => (prev - 1 + raceControlMessages.length) % raceControlMessages.length)}
            >
              &larr;
            </Button>
            <Button
              size="icon"
              variant="ghost"
              className="h-6 w-6 text-zinc-400 hover:text-white rounded hover:bg-white/5"
              onClick={() => setTickerIndex((prev) => (prev + 1) % raceControlMessages.length)}
            >
              &rarr;
            </Button>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <main className="flex-1 w-full max-w-7xl mx-auto px-8 py-10 relative z-10">
        
        {/* Error notification */}
        {error && (
          <div className="bg-red-950/30 border border-red-800/80 text-red-200 p-4 rounded-2xl mb-8 flex items-center gap-3 animate-fade-in backdrop-blur-md">
            <ShieldAlert className="h-5 w-5 text-red-400 shrink-0" />
            <div className="text-sm font-medium">{error}</div>
            <Button size="sm" variant="ghost" className="ml-auto text-xs text-red-200" onClick={fetchData}>
              Retry
            </Button>
          </div>
        )}

        {/* Lead Stats Board */}
        {!isLoading && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-10">
            {/* Stat 1 */}
            <Card className="bg-white/[0.02] border-white/5 backdrop-blur-md shadow-2xl relative overflow-hidden group rounded-2xl transition-all hover:border-white/10">
              <CardHeader className="pb-2">
                <CardDescription className="text-zinc-400 font-bold tracking-wider uppercase text-[10px]">Registered Teams</CardDescription>
                <CardTitle className="text-4xl font-black mt-1 text-white font-mono tracking-tight">
                  {dashboardStats.registeredTeamsCount}
                </CardTitle>
              </CardHeader>
              <CardContent className="text-xs text-zinc-500 font-semibold font-mono">
                F1: {teams.filter(t => t.type === "f1").length} |{' '}
                F2: {teams.filter(t => t.type === "f2").length} |{' '}
                FE: {teams.filter(t => t.type === "fe").length}
              </CardContent>
            </Card>

            {/* Stat 2 */}
            <Card className="bg-white/[0.02] border-white/5 backdrop-blur-md shadow-2xl relative overflow-hidden group rounded-2xl transition-all hover:border-white/10">
              <CardHeader className="pb-2">
                <CardDescription className="text-zinc-400 font-bold tracking-wider uppercase text-[10px]">Active Drivers</CardDescription>
                <CardTitle className="text-4xl font-black mt-1 text-white font-mono tracking-tight">{dashboardStats.activeDriversCount}</CardTitle>
              </CardHeader>
              <CardContent className="text-xs text-zinc-500 font-semibold font-mono">
                Avg Points: {dashboardStats.averagePoints} per Driver
              </CardContent>
            </Card>

            {/* Stat 3 */}
            <Card className="bg-white/[0.02] border-white/5 backdrop-blur-md shadow-2xl relative overflow-hidden group rounded-2xl transition-all hover:border-white/10">
              <CardHeader className="pb-2">
                <CardDescription className="text-zinc-400 font-bold tracking-wider uppercase text-[10px]">
                  {seriesFilter === "all" ? "Leading Team" : `Leading ${seriesFilter.toUpperCase()} Team`}
                </CardDescription>
                <CardTitle className="text-2xl font-bold tracking-tight text-white mt-2 truncate">
                  {dashboardStats.leadingTeamName}
                </CardTitle>
              </CardHeader>
              <CardContent className="text-xs text-zinc-500 font-semibold">
                <span className="font-mono">{dashboardStats.leadingTeamPoints} PTS</span>
                {' '}•{' '}
                <span className="font-mono">{dashboardStats.leadingTeamWins} WINS</span>
              </CardContent>
            </Card>

            {/* Stat 4 */}
            <Card className="bg-white/[0.02] border-white/5 backdrop-blur-md shadow-2xl relative overflow-hidden group rounded-2xl transition-all hover:border-white/10">
              <CardHeader className="pb-2">
                <CardDescription className="text-zinc-400 font-bold tracking-wider uppercase text-[10px]">
                  {seriesFilter === "all" ? "Leaderboard Pilot" : `Top ${seriesFilter.toUpperCase()} Pilot`}
                </CardDescription>
                <CardTitle className="text-2xl font-bold tracking-tight text-white mt-2 truncate">
                  {dashboardStats.leadingDriverName}
                </CardTitle>
              </CardHeader>
              <CardContent className="text-xs text-zinc-500 font-semibold font-mono">
                {dashboardStats.leadingDriverPoints} PTS &bull; CAR #{dashboardStats.leadingDriverNumber}
              </CardContent>
            </Card>
          </div>
        )}

        {/* Loading Board */}
        {isLoading && (
          <div className="flex flex-col items-center justify-center py-24 gap-4 text-zinc-400">
            <RefreshCw className="h-10 w-10 animate-spin text-orange-500" />
            <p className="text-sm font-semibold tracking-wider uppercase text-zinc-500">Retrieving stats board...</p>
          </div>
        )}

        {/* Tab Selection */}
        {!isLoading && (
          <Tabs defaultValue="teams" className="w-full">
            <div className="flex flex-col md:flex-row items-stretch md:items-center justify-between gap-6 mb-8 border-b border-white/5 pb-4">
              <TabsList className="bg-white/[0.02] border border-white/5 p-1 rounded-xl w-fit flex gap-1">
                <TabsTrigger value="teams" className="text-xs font-semibold px-4 py-2 rounded-lg text-zinc-400 hover:text-zinc-200 data-[state=active]:bg-white/10 data-[state=active]:text-white transition-all">
                  Teams Grid
                </TabsTrigger>
                <TabsTrigger value="drivers" className="text-xs font-semibold px-4 py-2 rounded-lg text-zinc-400 hover:text-zinc-200 data-[state=active]:bg-white/10 data-[state=active]:text-white transition-all">
                  Drivers List
                </TabsTrigger>
              </TabsList>

              <div className="flex items-center gap-3">
                <span className="text-[10px] uppercase font-bold text-zinc-500 tracking-wider">Series Classification:</span>
                <div className="flex rounded-xl overflow-hidden border border-white/5 bg-white/[0.02] p-1 gap-1">
                  {["all", "f1", "f2", "fe"].map(ser => (
                    <button
                      key={ser}
                      onClick={() => setSeriesFilter(ser)}
                      className={`px-3.5 py-1.5 text-[10px] rounded-lg font-bold uppercase tracking-wider transition-all ${
                        seriesFilter === ser
                          ? "bg-white/10 text-white shadow-sm"
                          : "text-zinc-400 hover:text-zinc-200"
                      }`}
                    >
                      {ser === "all" ? "All Class" : ser}
                    </button>
                  ))}
                </div>
              </div>
            </div>

            {/* Teams Panel */}
            <TabsContent value="teams" className="outline-none">
              {filteredTeams.length === 0 ? (
                <div className="text-center py-20 border border-dashed border-white/10 rounded-2xl bg-white/[0.01] backdrop-blur-sm">
                  <Compass className="h-12 w-12 mx-auto text-zinc-600 mb-4" />
                  <p className="text-sm text-zinc-400 font-semibold uppercase tracking-wider">No teams registered under this series</p>
                  <Button variant="link" onClick={handleOpenTeamAdd} className="text-xs text-orange-500 font-bold mt-2 hover:text-orange-400">
                    Add Team to Paddock
                  </Button>
                </div>
              ) : (
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
                  {filteredTeams.map(team => {
                    const isHovered = hoveredCardId === team.id;
                    const visibleColor = team.visible_color || "#3b82f6";
                    return (
                      <Card
                        key={team.id}
                        onMouseEnter={() => setHoveredCardId(team.id)}
                        onMouseLeave={() => setHoveredCardId(null)}
                        className="bg-zinc-950/30 border border-white/5 hover:border-white/10 transition-all duration-300 shadow-xl relative overflow-hidden rounded-2xl flex flex-col justify-between"
                        style={{
                          borderLeft: `4px solid ${visibleColor}`,
                          boxShadow: isHovered ? `0 15px 35px -15px ${visibleColor}25` : 'none',
                          borderColor: isHovered ? `${visibleColor}44` : 'rgba(255,255,255,0.05)'
                        }}
                      >
                        {/* Soft Glow Radial Accent */}
                        <div
                          className="absolute inset-0 opacity-[0.01] pointer-events-none transition-all duration-300"
                          style={{
                            background: `radial-gradient(circle at top right, ${visibleColor}, transparent 60%)`,
                            opacity: isHovered ? 0.05 : 0.01
                          }}
                        />

                        <CardHeader className="pb-4 relative z-10">
                          <div className="flex items-start justify-between">
                            <div>
                              <div className="flex items-center gap-2 flex-wrap">
                                <span
                                  className="text-[8px] border rounded-md px-2 py-0.5 font-extrabold uppercase tracking-widest bg-white/[0.02]"
                                  style={{
                                    borderColor: `${visibleColor}44`,
                                    color: visibleColor
                                  }}
                                >
                                  {getSeriesDisplay(team.type)}
                                </span>
                              </div>
                              <CardTitle className="text-xl font-extrabold text-white mt-3 tracking-tight">{team.teamName}</CardTitle>
                            </div>
                            <div className="flex gap-1 bg-white/[0.02] border border-white/5 rounded-xl p-0.5">
                              <Button
                                size="icon"
                                variant="ghost"
                                className="h-8 w-8 text-zinc-400 hover:text-white hover:bg-white/5 rounded-lg"
                                onClick={() => handleOpenTeamEdit(team)}
                              >
                                <Edit2 className="h-3.5 w-3.5" />
                              </Button>
                              <Button
                                size="icon"
                                variant="ghost"
                                className="h-8 w-8 text-zinc-400 hover:text-red-400 hover:bg-red-500/10 rounded-lg"
                                onClick={() => handleDeleteTeam(team.id)}
                              >
                                <Trash2 className="h-3.5 w-3.5" />
                              </Button>
                            </div>
                          </div>
                          
                          <div className="space-y-1 mt-2 text-xs text-zinc-400 font-medium">
                            <div className="flex items-center gap-1.5">
                              <MapPin className="h-3 w-3 text-zinc-500" />
                              <span>HQ: {team.headquarters}</span>
                            </div>
                            <div className="flex items-center gap-1.5">
                              <Users className="h-3 w-3 text-zinc-500" />
                              <span>Principal: {team.principalName}</span>
                            </div>
                          </div>
                        </CardHeader>

                        <CardContent className="pb-4 relative z-10 flex-1">
                          {/* Stats Grid */}
                          <div className="grid grid-cols-3 gap-2 bg-white/[0.01] border border-white/5 rounded-xl p-3 mb-4 text-center">
                            <div>
                              <div className="text-[9px] text-zinc-500 font-bold tracking-widest uppercase">Points</div>
                              <div className="text-base font-black text-white mt-0.5 font-mono tracking-wider">{team.championshipPoints}</div>
                            </div>
                            <div>
                              <div className="text-[9px] text-zinc-500 font-bold tracking-widest uppercase">Wins</div>
                              <div className="text-base font-black text-white mt-0.5 font-mono tracking-wider">{team.raceWins}</div>
                            </div>
                            <div>
                              <div className="text-[9px] text-zinc-500 font-bold tracking-widest uppercase">Podiums</div>
                              <div className="text-base font-black text-white mt-0.5 font-mono tracking-wider">{team.podiums}</div>
                            </div>
                          </div>

                          {/* Specifications Block */}
                          <div className="border-t border-white/5 pt-3.5 space-y-2 text-xs">
                            {team.type === "f1" && (
                              <>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5"><Gauge className="h-3.5 w-3.5 text-orange-500" /> Power Unit</span>
                                  <span className="font-semibold text-zinc-200">{team.powerUnit}</span>
                                </div>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5"><Landmark className="h-3.5 w-3.5 text-orange-500" /> Budget Cap</span>
                                  <span className="font-semibold text-zinc-200">${team.budgetCapMln}M</span>
                                </div>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5"><Trophy className="h-3.5 w-3.5 text-orange-500" /> Constructor Pos</span>
                                  <span className="font-semibold text-zinc-200">P{team.constructorPos}</span>
                                </div>
                                <div className="flex justify-between items-center pt-2 border-t border-dashed border-white/5">
                                  <span className="text-zinc-500 font-medium">Engine Classification:</span>
                                  <span className="font-bold text-amber-400 text-[11px]">{team.engine_status}</span>
                                </div>
                              </>
                            )}

                            {team.type === "f2" && (
                              <>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5"><Gauge className="h-3.5 w-3.5 text-orange-500" /> Chassis Model</span>
                                  <span className="font-semibold text-zinc-200">{team.chassisModel}</span>
                                </div>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5"><Users className="h-3.5 w-3.5 text-orange-500" /> F1 Graduates</span>
                                  <span className="font-semibold text-zinc-200">{team.f1Graduates} drivers</span>
                                </div>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5"><Sparkles className="h-3.5 w-3.5 text-orange-500" /> Feeder Status</span>
                                  <span className="font-semibold text-zinc-200">{team.isFeederSeries ? "Feeder Team" : "Independent"}</span>
                                </div>
                                <div className="flex justify-between items-center pt-2 border-t border-dashed border-white/5">
                                  <span className="text-zinc-500 font-medium">Academy Class:</span>
                                  <span className="font-bold text-cyan-400 text-[11px]">{team.feeder_status_text}</span>
                                </div>
                              </>
                            )}

                            {team.type === "fe" && (
                              <>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5"><Zap className="h-3.5 w-3.5 text-orange-500" /> Energy Partner</span>
                                  <span className="font-semibold text-zinc-200">{team.energyPartner}</span>
                                </div>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5">
                                    <Gauge className="h-3.5 w-3.5 text-orange-500" /> Battery Capacity
                                  </span>
                                  <span className="font-semibold text-zinc-200">{team.batteryCapacityKwh} kWh</span>
                                </div>
                                <div className="flex justify-between items-center">
                                  <span className="text-zinc-400 flex items-center gap-1.5">
                                    <Sparkles className="h-3.5 w-3.5 text-orange-500" /> Eco Rating
                                  </span>
                                  <span className="font-semibold text-zinc-200">
                                    {team.sustainabilityScore}/100
                                  </span>
                                </div>
                                <div className="flex justify-between items-center pt-2 border-t border-dashed border-white/5">
                                  <span className="text-zinc-500 font-medium">E-Tech Status:</span>
                                  <span className="font-bold text-emerald-400 text-[11px]">
                                    {team.battery_rating_text}
                                  </span>
                                </div>
                              </>
                            )}
                          </div>

                          {/* Lineup Pit-Garage Block */}
                          <div className="border-t border-white/5 mt-4 pt-3.5">
                            <span className="text-[9px] text-zinc-500 font-bold uppercase tracking-widest block mb-2">PIT GARAGE ALLOCATION</span>
                            <div className="grid grid-cols-2 gap-2.5">
                              {/* GARAGE SLOT 1 */}
                              <div className="border border-white/5 bg-zinc-950/40 rounded-xl p-2.5 font-mono text-[10px] relative overflow-hidden flex flex-col justify-between min-h-[62px]">
                                <div className="text-[7.5px] text-zinc-500 uppercase tracking-widest font-extrabold">GARAGE SLOT 1</div>
                                {team.drivers[0] ? (
                                  <div className="mt-1">
                                    <div className="font-black text-zinc-100 truncate tracking-tight text-[11px]">
                                      #{team.drivers[0].racingNumber} {team.drivers[0].lastName.toUpperCase()}
                                    </div>
                                    <div className="text-[8.5px] text-zinc-400 mt-0.5">
                                      POINTS:{' '}
                                      <span className="text-orange-500 font-bold font-mono">
                                        {team.drivers[0].points}
                                      </span>
                                    </div>
                                  </div>
                                ) : (
                                  <div className="text-zinc-600 italic font-medium tracking-wide mt-1.5 uppercase text-[9px]">VACANT</div>
                                )}
                              </div>
                              {/* GARAGE SLOT 2 */}
                              <div className="border border-white/5 bg-zinc-950/40 rounded-xl p-2.5 font-mono text-[10px] relative overflow-hidden flex flex-col justify-between min-h-[62px]">
                                <div className="text-[7.5px] text-zinc-500 uppercase tracking-widest font-extrabold">GARAGE SLOT 2</div>
                                {team.drivers[1] ? (
                                  <div className="mt-1">
                                    <div className="font-black text-zinc-100 truncate tracking-tight text-[11px]">
                                      #{team.drivers[1].racingNumber} {team.drivers[1].lastName.toUpperCase()}
                                    </div>
                                    <div className="text-[8.5px] text-zinc-400 mt-0.5">
                                      POINTS:{' '}
                                      <span className="text-orange-500 font-bold font-mono">
                                        {team.drivers[1].points}
                                      </span>
                                    </div>
                                  </div>
                                ) : (
                                  <div className="text-zinc-600 italic font-medium tracking-wide mt-1.5 uppercase text-[9px]">VACANT</div>
                                )}
                              </div>
                            </div>
                            {team.drivers.length > 2 && (
                              <div className="mt-1.5 text-[8.5px] text-zinc-500 font-mono tracking-wide text-right">
                                + {team.drivers.length - 2} RESERVE PILOTS SIGNED
                              </div>
                            )}
                          </div>
                        </CardContent>

                        <CardFooter className="pt-3 border-t border-white/5 relative z-10">
                          <Button
                            variant="secondary"
                            size="sm"
                            className="w-full text-xs font-bold bg-white/[0.02] hover:bg-white/[0.08] text-white hover:text-orange-500 border border-white/5 rounded-xl transition-all"
                            onClick={() => handleOpenSimulate(team)}
                          >
                            <Play className="mr-1.5 h-3.5 w-3.5 text-orange-500 fill-orange-500" />
                            Simulate Race
                          </Button>
                        </CardFooter>
                      </Card>
                    );
                  })}
                </div>
              )}
            </TabsContent>

            {/* Drivers Panel */}
            <TabsContent value="drivers" className="outline-none">
              <div className="bg-zinc-950/40 border border-white/5 rounded-2xl backdrop-blur-md shadow-2xl p-8">
                <div className="flex items-center justify-between mb-6">
                  <div>
                    <h3 className="text-lg font-bold text-white tracking-tight">Driver Standings</h3>
                    <p className="text-xs text-zinc-400 font-medium">Championship classification points and contract teams.</p>
                  </div>
                  <Button
                    size="sm"
                    onClick={handleOpenDriverAdd}
                    className="bg-white/[0.02] hover:bg-white/[0.08] text-white text-xs border border-white/10 rounded-xl font-bold px-4 py-2"
                  >
                    <Plus className="mr-1.5 h-3.5 w-3.5 text-orange-500" />
                    Sign Driver
                  </Button>
                </div>

                <div className="overflow-hidden rounded-xl border border-white/5">
                  <Table>
                    <TableHeader className="border-white/5 bg-white/[0.02]">
                      <TableRow className="hover:bg-transparent border-white/5">
                        <TableHead className="text-zinc-400 font-bold uppercase text-[9px] tracking-wider w-[100px] text-center">Rank</TableHead>
                        <TableHead className="text-zinc-400 font-bold uppercase text-[9px] tracking-wider">Pilot Name</TableHead>
                        <TableHead className="text-zinc-400 font-bold uppercase text-[9px] tracking-wider w-[120px] text-center">Car No.</TableHead>
                        <TableHead className="text-zinc-400 font-bold uppercase text-[9px] tracking-wider">Racing Team</TableHead>
                        <TableHead className="text-zinc-400 font-bold uppercase text-[9px] tracking-wider text-right w-[150px] pr-8">Championship Points</TableHead>
                        <TableHead className="text-zinc-400 font-bold uppercase text-[9px] tracking-wider w-[100px]" />
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {drivers
                        .filter(d => {
                          if (seriesFilter === "all") return true;
                          const t = teams.find(team => team.id === d.teamId);
                          return t?.type === seriesFilter;
                        })
                        .sort((a, b) => b.points - a.points)
                        .map((driver, index) => {
                          const driverTeam = teams.find(t => t.id === driver.teamId);
                          const isTopThree = index < 3;
                          const rankColor = index === 0 
                            ? "bg-amber-500/20 text-amber-300 border-amber-500/40" 
                            : index === 1 
                              ? "bg-slate-400/20 text-slate-300 border-slate-400/40" 
                              : "bg-amber-800/20 text-amber-600 border-amber-800/40";
                          return (
                            <TableRow key={driver.id} className="border-white/[0.02] hover:bg-white/[0.01]">
                              <TableCell className="text-center">
                                {isTopThree ? (
                                  <span className={`inline-flex items-center justify-center font-black text-[10px] w-6 h-6 rounded-full border ${rankColor} font-mono`}>
                                    {index + 1}
                                  </span>
                                ) : (
                                  <span className="font-bold text-zinc-500 text-xs font-mono">{index + 1}</span>
                                )}
                              </TableCell>
                              <TableCell className="font-extrabold text-sm text-white">
                                {driver.firstName} {driver.lastName}
                              </TableCell>
                              <TableCell className="font-mono text-center font-bold text-xs text-zinc-400">
                                #{driver.racingNumber}
                              </TableCell>
                              <TableCell>
                                {driverTeam ? (
                                  <div className="flex items-center gap-2">
                                    <div className="h-2.5 w-2.5 rounded-full border border-white/10" style={{ backgroundColor: driverTeam.visible_color || "#3b82f6" }} />
                                    <span className="text-xs text-zinc-300 font-semibold">{driverTeam.teamName}</span>
                                    <span className="text-[9px] bg-white/5 border border-white/5 rounded px-1 text-zinc-500 font-bold uppercase">{driverTeam.type}</span>
                                  </div>
                                ) : (
                                  <span className="text-xs text-zinc-600 italic">Unassigned</span>
                                )}
                              </TableCell>
                              <TableCell className="font-bold font-mono tracking-wider text-sm text-white text-right pr-8">{driver.points} PTS</TableCell>
                              <TableCell className="pr-4">
                                <div className="flex justify-end gap-1.5">
                                  <Button
                                    size="icon"
                                    variant="ghost"
                                    className="h-8 w-8 text-zinc-400 hover:text-white hover:bg-white/5 rounded-lg"
                                    onClick={() => handleOpenDriverEdit(driver)}
                                  >
                                    <Edit2 className="h-3.5 w-3.5" />
                                  </Button>
                                  <Button
                                    size="icon"
                                    variant="ghost"
                                    className="h-8 w-8 text-zinc-400 hover:text-red-400 hover:bg-red-500/10 rounded-lg"
                                    onClick={() => handleDeleteDriver(driver.id)}
                                  >
                                    <Trash2 className="h-3.5 w-3.5" />
                                  </Button>
                                </div>
                              </TableCell>
                            </TableRow>
                          );
                        })}
                    </TableBody>
                  </Table>
                </div>
              </div>
            </TabsContent>
          </Tabs>
        )}
      </main>

      {/* Footer */}
      <footer className="border-t border-white/5 bg-zinc-950 py-6 text-center text-[10px] text-zinc-500 font-bold tracking-widest uppercase">
        FORMULA RACING &bull; Telemetry Engine V2 &bull; &copy; 2026
      </footer>

      {/* --- ADD/EDIT TEAM DIALOG --- */}
      <Dialog open={isTeamModalOpen} onOpenChange={setIsTeamModalOpen}>
        <DialogContent className="bg-zinc-950 border border-white/10 text-zinc-100 max-w-lg rounded-2xl shadow-2xl backdrop-blur-xl">
          <form onSubmit={handleSaveTeam}>
            <DialogHeader>
              <DialogTitle className="text-white font-black text-lg tracking-tight">
                {editingTeam ? "Modify Team Specifications" : "Register New Racing Team"}
              </DialogTitle>
              <DialogDescription className="text-zinc-400 text-xs">
                Fill out the series credentials and branding elements below.
              </DialogDescription>
            </DialogHeader>

            <div className="grid gap-4 py-6 border-y border-white/5 my-4">
              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="t-type" className="text-right text-xs font-bold text-zinc-400">Class Series</Label>
                <div className="col-span-3">
                  <Select
                    value={teamType}
                    onValueChange={(val: any) => setTeamType(val)}
                    disabled={!!editingTeam}
                  >
                    <SelectTrigger className="bg-zinc-900 border-white/5 text-zinc-300 rounded-xl focus:ring-orange-500" id="t-type">
                      <SelectValue placeholder="Select Series" />
                    </SelectTrigger>
                    <SelectContent className="bg-zinc-900 border-white/10 text-zinc-300 rounded-xl">
                      <SelectItem value="f1">Formula 1</SelectItem>
                      <SelectItem value="f2">Formula 2</SelectItem>
                      <SelectItem value="fe">Formula E</SelectItem>
                    </SelectContent>
                  </Select>
                </div>
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="t-name" className="text-right text-xs font-bold text-zinc-400">Team Name</Label>
                <Input
                  id="t-name"
                  value={teamName}
                  onChange={e => setTeamName(e.target.value)}
                  className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500 focus:ring-0"
                  required
                />
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="t-principal" className="text-right text-xs font-bold text-zinc-400">Principal</Label>
                <Input
                  id="t-principal"
                  value={principalName}
                  onChange={e => setPrincipalName(e.target.value)}
                  className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500 focus:ring-0"
                  required
                />
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="t-hq" className="text-right text-xs font-bold text-zinc-400">Headquarters</Label>
                <Input
                  id="t-hq"
                  value={headquarters}
                  onChange={e => setHeadquarters(e.target.value)}
                  className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                  required
                />
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="t-color" className="text-right text-xs font-bold text-zinc-400">Brand Color</Label>
                <div className="col-span-3 flex gap-3 items-center">
                  <Input
                    id="t-color"
                    type="color"
                    value={teamColor}
                    onChange={e => setTeamColor(e.target.value)}
                    className="w-12 h-9 p-1 bg-zinc-900 border-white/5 rounded-xl cursor-pointer"
                  />
                  <span className="text-xs text-zinc-400 font-mono font-bold">{teamColor}</span>
                </div>
              </div>

              <div className="grid grid-cols-3 gap-3 pt-2">
                <div>
                  <Label htmlFor="t-points" className="text-[9px] uppercase font-bold text-zinc-500 tracking-wider">Points</Label>
                  <Input
                    id="t-points"
                    type="number"
                    value={championshipPoints}
                    onChange={e => setChampionshipPoints(Number(e.target.value))}
                    className="bg-zinc-900 border-white/5 rounded-xl mt-1.5 focus:border-orange-500"
                  />
                </div>
                <div>
                  <Label htmlFor="t-wins" className="text-[9px] uppercase font-bold text-zinc-500 tracking-wider">Wins</Label>
                  <Input
                    id="t-wins"
                    type="number"
                    value={raceWins}
                    onChange={e => setRaceWins(Number(e.target.value))}
                    className="bg-zinc-900 border-white/5 rounded-xl mt-1.5 focus:border-orange-500"
                  />
                </div>
                <div>
                  <Label htmlFor="t-podiums" className="text-[9px] uppercase font-bold text-zinc-500 tracking-wider">Podiums</Label>
                  <Input
                    id="t-podiums"
                    type="number"
                    value={podiums}
                    onChange={e => setPodiums(Number(e.target.value))}
                    className="bg-zinc-900 border-white/5 rounded-xl mt-1.5 focus:border-orange-500"
                  />
                </div>
              </div>

              {/* Dynamic Type Specs */}
              <div className="pt-2">
                <span className="text-[10px] font-bold text-zinc-500 uppercase tracking-widest block mb-3">
                  {getSeriesDisplay(teamType)} Parameters
                </span>

                {teamType === "f1" && (
                  <div className="space-y-3">
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-pu" className="text-right text-xs font-bold text-zinc-400">Power Unit</Label>
                      <Input
                        id="t-pu"
                        value={powerUnit}
                        onChange={e => setPowerUnit(e.target.value)}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-budget" className="text-right text-xs font-bold text-zinc-400">Budget ($M)</Label>
                      <Input
                        id="t-budget"
                        type="number"
                        step="0.1"
                        value={budgetCapMln}
                        onChange={e => setBudgetCapMln(Number(e.target.value))}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-pos" className="text-right text-xs font-bold text-zinc-400">Constructor Pos</Label>
                      <Input
                        id="t-pos"
                        type="number"
                        value={constructorPos}
                        onChange={e => setConstructorPos(Number(e.target.value))}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                  </div>
                )}

                {teamType === "f2" && (
                  <div className="space-y-3">
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-chassis" className="text-right text-xs font-bold text-zinc-400">Chassis</Label>
                      <Input
                        id="t-chassis"
                        value={chassisModel}
                        onChange={e => setChassisModel(e.target.value)}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-grads" className="text-right text-xs font-bold text-zinc-400">F1 Graduates</Label>
                      <Input
                        id="t-grads"
                        type="number"
                        value={f1Graduates}
                        onChange={e => setF1Graduates(Number(e.target.value))}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-feeder" className="text-right text-xs font-bold text-zinc-400">Is Feeder</Label>
                      <div className="col-span-3 flex items-center">
                        <input
                          id="t-feeder"
                          type="checkbox"
                          checked={isFeederSeries}
                          onChange={e => setIsFeederSeries(e.target.checked)}
                          className="w-4 h-4 text-orange-500 bg-zinc-900 border-white/5 rounded-lg focus:ring-0 cursor-pointer"
                        />
                      </div>
                    </div>
                  </div>
                )}

                {teamType === "fe" && (
                  <div className="space-y-3">
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-energy" className="text-right text-xs font-bold text-zinc-400">Energy Partner</Label>
                      <Input
                        id="t-energy"
                        value={energyPartner}
                        onChange={e => setEnergyPartner(e.target.value)}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-battery" className="text-right text-xs font-bold text-zinc-400">Battery (kWh)</Label>
                      <Input
                        id="t-battery"
                        type="number"
                        step="0.1"
                        value={batteryCapacityKwh}
                        onChange={e => setBatteryCapacityKwh(Number(e.target.value))}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                    <div className="grid grid-cols-4 items-center gap-4">
                      <Label htmlFor="t-eco" className="text-right text-xs font-bold text-zinc-400">Sustainability</Label>
                      <Input
                        id="t-eco"
                        type="number"
                        value={sustainabilityScore}
                        onChange={e => setSustainabilityScore(Number(e.target.value))}
                        className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                      />
                    </div>
                  </div>
                )}
              </div>
            </div>

            <DialogFooter>
              <Button type="button" variant="outline" className="border-white/10 hover:bg-white/5 rounded-xl text-xs font-semibold" onClick={() => setIsTeamModalOpen(false)}>
                Cancel
              </Button>
              <Button type="submit" className="bg-gradient-to-r from-amber-500 to-orange-600 text-white text-xs font-bold px-4 rounded-xl">
                Commit Specs
              </Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>

      {/* --- ADD/EDIT DRIVER DIALOG --- */}
      <Dialog open={isDriverModalOpen} onOpenChange={setIsDriverModalOpen}>
        <DialogContent className="bg-zinc-950 border border-white/10 text-zinc-100 max-w-md rounded-2xl shadow-2xl backdrop-blur-xl">
          <form onSubmit={handleSaveDriver}>
            <DialogHeader>
              <DialogTitle className="text-white font-black text-lg tracking-tight">
                {editingDriver ? "Amend Driver Credentials" : "Sign New Pilot"}
              </DialogTitle>
              <DialogDescription className="text-zinc-400 text-xs">
                Register contract agreement and pilot details.
              </DialogDescription>
            </DialogHeader>

            <div className="grid gap-4 py-6 border-y border-white/5 my-4">
              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="d-fname" className="text-right text-xs font-bold text-zinc-400">First Name</Label>
                <Input
                  id="d-fname"
                  value={driverFirstName}
                  onChange={e => setDriverFirstName(e.target.value)}
                  className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                  required
                />
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="d-lname" className="text-right text-xs font-bold text-zinc-400">Last Name</Label>
                <Input
                  id="d-lname"
                  value={driverLastName}
                  onChange={e => setDriverLastName(e.target.value)}
                  className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                  required
                />
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="d-number" className="text-right text-xs font-bold text-zinc-400">Car Number</Label>
                <Input
                  id="d-number"
                  type="number"
                  value={driverNumber}
                  onChange={e => setDriverNumber(Number(e.target.value))}
                  className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                  required
                />
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="d-points" className="text-right text-xs font-bold text-zinc-400">Points</Label>
                <Input
                  id="d-points"
                  type="number"
                  value={driverPoints}
                  onChange={e => setDriverPoints(Number(e.target.value))}
                  className="col-span-3 bg-zinc-900 border-white/5 rounded-xl focus:border-orange-500"
                  required
                />
              </div>

              <div className="grid grid-cols-4 items-center gap-4">
                <Label htmlFor="d-team" className="text-right text-xs font-bold text-zinc-400">Racing Team</Label>
                <div className="col-span-3">
                  <Select
                    value={driverTeamId}
                    onValueChange={(val) => setDriverTeamId(val)}
                  >
                    <SelectTrigger className="bg-zinc-900 border-white/5 text-zinc-300 rounded-xl focus:ring-orange-500" id="d-team">
                      <SelectValue placeholder="Contract Team" />
                    </SelectTrigger>
                    <SelectContent className="bg-zinc-900 border-white/10 text-zinc-300 rounded-xl">
                      {teams.map(t => (
                        <SelectItem key={t.id} value={t.id.toString()}>
                          {t.teamName} ({getSeriesDisplay(t.type)})
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                </div>
              </div>
            </div>

            <DialogFooter>
              <Button type="button" variant="outline" className="border-white/10 hover:bg-white/5 rounded-xl text-xs font-semibold" onClick={() => setIsDriverModalOpen(false)}>
                Cancel
              </Button>
              <Button type="submit" className="bg-gradient-to-r from-amber-500 to-orange-600 text-white text-xs font-bold px-4 rounded-xl">
                Sign Driver
              </Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>

      {/* --- SIMULATE RACE DIALOG --- */}
      <Dialog open={isSimulateModalOpen} onOpenChange={setIsSimulateModalOpen}>
        <DialogContent className="bg-zinc-950 border border-white/10 text-zinc-100 max-w-sm rounded-2xl shadow-2xl backdrop-blur-xl">
          <DialogHeader>
            <DialogTitle className="text-white font-black text-lg tracking-tight">Simulate Finish</DialogTitle>
            <DialogDescription className="text-zinc-400 text-xs">
              Simulate a championship placement for <strong>{simulatingTeam?.teamName}</strong>.
            </DialogDescription>
          </DialogHeader>

          <div className="py-6 space-y-4">
            <p className="text-xs text-zinc-500 leading-relaxed">
              Triggers the polymorphic calculations defined in C# (e.g. 25 points for F1 vs 15 points for F2) and recalculates constructor totals in PostgreSQL.
            </p>
            <div className="grid grid-cols-4 items-center gap-4">
              <Label htmlFor="sim-driver" className="text-xs font-bold text-zinc-400">Driver</Label>
              <div className="col-span-3">
                {simulatingTeam?.drivers && simulatingTeam.drivers.length > 0 ? (
                  <Select
                    value={simulatingDriverId}
                    onValueChange={setSimulatingDriverId}
                  >
                    <SelectTrigger className="bg-zinc-900 border-white/5 text-zinc-300 rounded-xl focus:ring-orange-500" id="sim-driver">
                      <SelectValue placeholder="Select Driver" />
                    </SelectTrigger>
                    <SelectContent className="bg-zinc-900 border-white/10 text-zinc-300 rounded-xl">
                      {simulatingTeam.drivers.map(d => (
                        <SelectItem key={d.id} value={d.id.toString()}>
                          {d.firstName} {d.lastName} (#{d.racingNumber})
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                ) : (
                  <span className="text-[11px] text-red-400 font-semibold italic">No drivers signed with team</span>
                )}
              </div>
            </div>

            <div className="grid grid-cols-4 items-center gap-4">
              <Label htmlFor="sim-pos" className="text-xs font-bold text-zinc-400">Position</Label>
              <div className="col-span-3">
                <Select
                  value={simPosition.toString()}
                  onValueChange={(val) => setSimPosition(Number(val))}
                >
                  <SelectTrigger className="bg-zinc-900 border-white/5 text-zinc-300 rounded-xl focus:ring-orange-500" id="sim-pos">
                    <SelectValue placeholder="Position" />
                  </SelectTrigger>
                  <SelectContent className="bg-zinc-900 border-white/10 text-zinc-300 rounded-xl">
                    {[1,2,3,4,5,6,7,8,9,10].map(pos => (
                      <SelectItem key={pos} value={pos.toString()}>Position P{pos}</SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
            </div>
          </div>

          <DialogFooter>
            <Button variant="outline" className="border-white/10 hover:bg-white/5 rounded-xl text-xs font-semibold" onClick={() => setIsSimulateModalOpen(false)}>
              Cancel
            </Button>
            <Button onClick={handleRunSimulation} className="bg-orange-600 hover:bg-orange-700 text-white text-xs font-bold rounded-xl px-4">
              Run Telemetry
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* --- COMPARE TEAMS DIALOG --- */}
      <Dialog open={isCompareModalOpen} onOpenChange={setIsCompareModalOpen}>
        <DialogContent className="bg-zinc-950 border border-white/10 text-zinc-100 max-w-md rounded-2xl shadow-2xl backdrop-blur-xl">
          <DialogHeader>
            <DialogTitle className="text-white font-black text-lg tracking-tight">Compare Standings</DialogTitle>
            <DialogDescription className="text-zinc-400 text-xs">
              Runs comparison rules using C# operator overloading (<code>&gt;</code> and <code>&lt;</code>).
            </DialogDescription>
          </DialogHeader>

          <div className="grid gap-4 py-6">
            <div className="grid grid-cols-2 gap-4">
              <div>
                <Label htmlFor="comp-a" className="text-xs font-bold text-zinc-400">Team Alpha</Label>
                <Select value={compareTeamA} onValueChange={setCompareTeamA}>
                  <SelectTrigger className="bg-zinc-900 border-white/5 text-zinc-300 mt-2 rounded-xl focus:ring-orange-500" id="comp-a">
                    <SelectValue placeholder="Select Team A" />
                  </SelectTrigger>
                  <SelectContent className="bg-zinc-900 border-white/10 text-zinc-300 rounded-xl">
                    {teams.map(t => (
                      <SelectItem key={t.id} value={t.id.toString()}>{t.teamName}</SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
              <div>
                <Label htmlFor="comp-b" className="text-xs font-bold text-zinc-400">Team Beta</Label>
                <Select value={compareTeamB} onValueChange={setCompareTeamB}>
                  <SelectTrigger className="bg-zinc-900 border-white/5 text-zinc-300 mt-2 rounded-xl focus:ring-orange-500" id="comp-b">
                    <SelectValue placeholder="Select Team B" />
                  </SelectTrigger>
                  <SelectContent className="bg-zinc-900 border-white/10 text-zinc-300 rounded-xl">
                    {teams.map(t => (
                      <SelectItem key={t.id} value={t.id.toString()}>{t.teamName}</SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
            </div>

            {compareResult && (
              <div className="mt-4 p-5 rounded-2xl bg-white/[0.01] border border-white/5 text-center animate-fade-in relative overflow-hidden">
                <Sparkles className="h-6 w-6 mx-auto text-amber-500 mb-2" />
                <h4 className="text-xs font-bold text-white uppercase tracking-widest mb-1">C# Operator Overload Output</h4>
                <p className="text-xs text-zinc-300 leading-relaxed font-semibold">{compareResult.message}</p>
                <div className="flex justify-around items-center mt-4 border-t border-white/5 pt-4 text-xs">
                  <div>
                    <span className="text-zinc-500 font-semibold text-[10px] uppercase tracking-wider">{compareResult.teamA.name}</span>
                    <div className="font-black text-white text-sm mt-0.5">{compareResult.teamA.points} pts</div>
                  </div>
                  <div className="text-zinc-700 font-black">VS</div>
                  <div>
                    <span className="text-zinc-500 font-semibold text-[10px] uppercase tracking-wider">{compareResult.teamB.name}</span>
                    <div className="font-black text-white text-sm mt-0.5">{compareResult.teamB.points} pts</div>
                  </div>
                </div>
              </div>
            )}
          </div>

          <DialogFooter>
            <Button variant="outline" className="border-white/10 hover:bg-white/5 rounded-xl text-xs font-semibold" onClick={() => setIsCompareModalOpen(false)}>
              Cancel
            </Button>
            <Button onClick={handleRunComparison} className="bg-orange-600 hover:bg-orange-700 text-white text-xs font-bold rounded-xl px-4">
              Compare Standings
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* --- GLOBAL GP SIMULATION DIALOG --- */}
      <Dialog open={isGlobalSimModalOpen} onOpenChange={setIsGlobalSimModalOpen}>
        <DialogContent className="bg-zinc-950 border border-white/10 text-zinc-100 max-w-xl rounded-2xl shadow-2xl backdrop-blur-xl">
          <DialogHeader>
            <DialogTitle className="text-white font-black text-lg tracking-tight">Global GP Simulation</DialogTitle>
            <DialogDescription className="text-zinc-400 text-xs">
              Simulate a full championship Grand Prix for all drivers of a series. Drag & drop drivers to set their finishing positions.
            </DialogDescription>
          </DialogHeader>

          <div className="grid gap-4 py-4">
            <div>
              <Label className="text-xs font-bold text-zinc-400">Select League Series</Label>
              <Select value={simSeries} onValueChange={(val: any) => setSimSeries(val)}>
                <SelectTrigger className="bg-zinc-900 border-white/5 text-zinc-300 mt-2 rounded-xl focus:ring-orange-500">
                  <SelectValue placeholder="Select Series" />
                </SelectTrigger>
                <SelectContent className="bg-zinc-900 border-white/10 text-zinc-300 rounded-xl">
                  <SelectItem value="f1">Formula 1</SelectItem>
                  <SelectItem value="f2">Formula 2</SelectItem>
                  <SelectItem value="fe">Formula E</SelectItem>
                </SelectContent>
              </Select>
            </div>

            <div className="mt-2">
              <span className="text-xs font-bold text-zinc-400 block mb-2">Drag to set positions (P1 down to P{simDrivers.length}):</span>
              <div className="max-h-[300px] overflow-y-auto space-y-1.5 pr-2 custom-scrollbar">
                {simDrivers.map((driver, index) => {
                  const driverTeam = teams.find(t => t.id === driver.teamId);
                  const isDragged = draggedIndex === index;
                  return (
                    <div
                      key={driver.id}
                      draggable
                      onDragStart={() => handleDragStart(index)}
                      onDragOver={handleDragOver}
                      onDrop={() => handleDrop(index)}
                      className={`flex items-center justify-between p-2.5 rounded-xl border transition-all cursor-grab active:cursor-grabbing select-none ${
                        isDragged 
                          ? "bg-orange-600/10 border-orange-500/50 opacity-40" 
                          : "bg-white/[0.01] border-white/5 hover:bg-white/[0.04] hover:border-white/10"
                      }`}
                    >
                      <div className="flex items-center gap-3">
                        <span className="font-mono text-[10px] font-black w-6 text-zinc-500">
                          P{index + 1}
                        </span>
                        <div>
                          <span className="text-xs font-bold text-white">{driver.firstName} {driver.lastName}</span>
                          <span className="text-[10px] text-zinc-500 ml-2 font-mono">#{driver.racingNumber}</span>
                        </div>
                      </div>
                      <div className="flex items-center gap-3">
                        {driverTeam && (
                          <span 
                            className="text-[9px] px-2 py-0.5 rounded font-mono font-semibold"
                            style={{ 
                              backgroundColor: `${driverTeam.visible_color}15`, 
                              color: driverTeam.visible_color 
                            }}
                          >
                            {driverTeam.teamName}
                          </span>
                        )}
                        <span className="text-xs font-mono font-bold text-zinc-400">{driver.points} pts</span>
                      </div>
                    </div>
                  );
                })}
                {simDrivers.length === 0 && (
                  <p className="text-xs text-zinc-500 py-6 text-center">No drivers found in this series. Add drivers first.</p>
                )}
              </div>
            </div>
          </div>

          <DialogFooter>
            <Button variant="outline" className="border-white/10 hover:bg-white/5 rounded-xl text-xs font-semibold" onClick={() => setIsGlobalSimModalOpen(false)}>
              Cancel
            </Button>
            <Button 
              onClick={handleRunGlobalSimulation} 
              disabled={simDrivers.length === 0}
              className="bg-emerald-600 hover:bg-emerald-700 disabled:opacity-50 text-white text-xs font-bold rounded-xl px-4"
            >
              Simulate & Save Standings
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  );
}

export default App;
