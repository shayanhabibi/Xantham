const assert = require('node:assert/strict');

function verify({ packageResult, full, verificationResult, verified }) {
  assert.equal(packageResult, 'success', 'Verified packages must succeed before publication');
  assert.equal(full, 'true', 'Publication requires full verification');
  assert.equal(verificationResult, 'success', 'Master Test evidence job must succeed');
  assert.equal(verified, 'true', 'Publication requires full exact-commit Test evidence');
}

if (require.main === module) {
  verify({ packageResult: process.env.PACKAGE_RESULT, full: process.env.FULL,
    verificationResult: process.env.VERIFICATION_RESULT, verified: process.env.VERIFIED });
}
module.exports = verify;
