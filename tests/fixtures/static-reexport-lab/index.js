export class Certificate {
    verify(spkac) {
        return spkac.length > 0;
    }

    static exportChallenge(spkac) {
        return spkac;
    }
}
