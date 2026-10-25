interface BotBase { score: number; verifiedBot: boolean; corporateProxy: boolean; staticResource: boolean; detectionIds: number[]; }
interface Bot { botManagement: BotBase; clientTrustScore: number; }
interface BotEnterprise extends Bot { botManagement: BotBase & { ja3Hash: string; }; }
interface Other { colo: string; }
type Combined = BotEnterprise & Other;
