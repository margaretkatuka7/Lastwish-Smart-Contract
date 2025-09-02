# 🏛️ Lastwill - Digital Will Execution Smart Contract

> 📜 A decentralized digital will system built on Stacks blockchain for secure inheritance management

## 🌟 Overview

Lastwill is a smart contract that enables users to create digital wills with automatic inheritance distribution. The contract ensures that assets are distributed to designated beneficiaries only after specific conditions are met, providing a trustless and transparent inheritance system.

## ✨ Features

- 📝 **Create Digital Wills**: Set up wills with multiple beneficiaries and custom amounts
- 💓 **Heartbeat System**: Regular check-ins to prove the testator is alive
- ⚰️ **Death Declaration**: Community-driven death verification system
- 💰 **Automatic Distribution**: Beneficiaries can claim their inheritance automatically
- 🔄 **Will Updates**: Modify beneficiaries and amounts while alive
- ❌ **Will Revocation**: Cancel wills and retrieve deposited funds
- ⏰ **Expiry Protection**: Wills expire if not claimed within specified timeframe

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run Clarinet commands to deploy and test

```bash
clarinet check
clarinet test
clarinet deploy
```

## 📖 Usage Guide

### 1. 📋 Creating a Will

```clarity
(contract-call? .lastwill create-will 
  (list 'SP1234... 'SP5678...) 
  (list u1000000 u500000) 
  u52560) ;; ~1 year expiry
```

### 2. 💓 Sending Heartbeat

```clarity
(contract-call? .lastwill heartbeat)
```

### 3. ⚰️ Declaring Death

Anyone can declare a testator dead if they haven't sent a heartbeat within the threshold:

```clarity
(contract-call? .lastwill declare-death 'SP1234...)
```

### 4. 💰 Claiming Inheritance

Beneficiaries can claim their inheritance after death declaration:

```clarity
(contract-call? .lastwill claim-inheritance 'SP1234...)
```

### 5. 🔄 Updating Will

```clarity
(contract-call? .lastwill update-will 
  (list 'SP1234... 'SP9999...) 
  (list u800000 u700000) 
  u52560)
```

### 6. ❌ Revoking Will

```clarity
(contract-call? .lastwill revoke-will)
```

## 🔍 Read-Only Functions

- `get-will`: Retrieve will details
- `get-testator-status`: Check testator's status
- `get-claim-status`: Check if inheritance was claimed
- `is-will-claimable`: Check if will can be claimed
- `get-inheritance-amount`: Get beneficiary's inheritance amount
- `get-contract-balance`: View total contract balance

## ⚙️ Configuration

### Heartbeat Threshold

The default heartbeat threshold is 144 blocks (~24 hours). Contract owner can modify:

```clarity
(contract-call? .lastwill set-heartbeat-threshold u1008) ;; ~1 week
```

## 🛡️ Security Features

- ✅ **Access Control**: Only testators can modify their own wills
- ✅ **Double Spending Protection**: Prevents multiple claims by same beneficiary
- ✅ **Time-based Validation**: Ensures proper timing for all operations
- ✅ **Balance Verification**: Validates sufficient funds before operations
- ✅ **Death Verification**: Community-based death declaration system

## 🧪 Testing

Run the test suite:

```bash
clarinet test
```

## 📊 Error Codes

| Code | Description |
|------|-------------|
| u100 | Unauthorized access |
| u101 | Will not found |
| u102 | Will already exists |
| u103 | Testator not deceased |
| u104 | Inheritance already claimed |
| u105 | Insufficient balance |
| u106 | Invalid beneficiary |
| u107 | Will expired |
| u108 | Too early to declare death |

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

## ⚠️ Disclaimer

This smart contract is for educational and experimental purposes. Always conduct thorough testing and audits before using in production environments. The authors are not responsible for any loss of funds# Lastwish-Smart-Contract

