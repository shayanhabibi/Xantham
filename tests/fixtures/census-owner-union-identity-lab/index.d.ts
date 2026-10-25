interface Auth { certPresented: "1"; certIssuerDN: string; certSubjectDN: string; }
interface Placeholder { certPresented: "0"; certIssuerDN: ""; }
interface Shield { tlsClientAuth: Auth | Placeholder; }
interface Other { colo: string; }
type Combined = Shield & Other;
