// This file is part of Substrate.

// Copyright (C) 2018-2022 Parity Technologies (UK) Ltd.
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

//! Substrate chain configurations.

use grandpa_primitives::AuthorityId as GrandpaId;
use kitchensink_runtime::{
	constants::currency::*, wasm_binary_unwrap, AuthorityDiscoveryConfig, BabeConfig,
	BalancesConfig, Block, CouncilConfig, DemocracyConfig, ElectionsConfig, GrandpaConfig,
	ImOnlineConfig, IndicesConfig, MaxNominations, NominationPoolsConfig, SessionConfig,
	SessionKeys, SocietyConfig, StakerStatus, StakingConfig, SudoConfig, SystemConfig,
	TechnicalCommitteeConfig,
};
use pallet_im_online::sr25519::AuthorityId as ImOnlineId;
use sc_chain_spec::ChainSpecExtension;
use sc_service::ChainType;
use sc_telemetry::TelemetryEndpoints;
use serde::{Deserialize, Serialize};
use sp_authority_discovery::AuthorityId as AuthorityDiscoveryId;
use sp_consensus_babe::AuthorityId as BabeId;
use sp_core::{crypto::UncheckedInto, sr25519, Pair, Public};
use sp_runtime::{
	traits::{IdentifyAccount, Verify},
	Perbill,
};
use sp_core::OpaquePeerId; // A struct wraps Vec<u8>, represents as our `PeerId`.
use kitchensink_runtime::NodeAuthorizationConfig; // The genesis config that serves for our pallet.

pub use kitchensink_runtime::GenesisConfig;
pub use node_primitives::{AccountId, Balance, Signature};

type AccountPublic = <Signature as Verify>::Signer;

const STAGING_TELEMETRY_URL: &str = "wss://telemetry.polkadot.io/submit/";

/// Node `ChainSpec` extensions.
///
/// Additional parameters for some Substrate core modules,
/// customizable from the chain spec.
#[derive(Default, Clone, Serialize, Deserialize, ChainSpecExtension)]
#[serde(rename_all = "camelCase")]
pub struct Extensions {
	/// Block numbers with known hashes.
	pub fork_blocks: sc_client_api::ForkBlocks<Block>,
	/// Known bad block hashes.
	pub bad_blocks: sc_client_api::BadBlocks<Block>,
	/// The light sync state extension used by the sync-state rpc.
	pub light_sync_state: sc_sync_state_rpc::LightSyncStateExtension,
}

/// Specialized `ChainSpec`.
pub type ChainSpec = sc_service::GenericChainSpec<GenesisConfig, Extensions>;
/// Flaming Fir testnet generator
pub fn flaming_fir_config() -> Result<ChainSpec, String> {
	ChainSpec::from_json_bytes(&include_bytes!("../res/flaming-fir.json")[..])
}

fn session_keys(
	grandpa: GrandpaId,
	babe: BabeId,
	im_online: ImOnlineId,
	authority_discovery: AuthorityDiscoveryId,
) -> SessionKeys {
	SessionKeys { grandpa, babe, im_online, authority_discovery }
}

fn staging_testnet_config_genesis() -> GenesisConfig {
	#[rustfmt::skip]
	// stash, controller, session-key
	// generated with secret:
	// for i in 1 2 3 4 ; do for j in stash controller; do subkey inspect "$secret"/fir/$j/$i; done; done
	//
	// and
	//
	// for i in 1 2 3 4 ; do for j in session; do subkey --ed25519 inspect "$secret"//fir//$j//$i; done; done

	let initial_authorities: Vec<(
		AccountId,
		AccountId,
		GrandpaId,
		BabeId,
		ImOnlineId,
		AuthorityDiscoveryId,
	)> = vec![
		(
			// 5GuQdepeV7b2hirjDh9xFmZxkS5ZqacLMnFrRn3THoqJa3wg
			array_bytes::hex_n_into_unchecked("d615f9e0ffa69a0814852952b614a7c1b9f361454188caca560cca3f1cfaa16f"),
			// 5GuQdepeV7b2hirjDh9xFmZxkS5ZqacLMnFrRn3THoqJa3wg
			array_bytes::hex_n_into_unchecked("d615f9e0ffa69a0814852952b614a7c1b9f361454188caca560cca3f1cfaa16f"),
			// 5Fb9ayurnxnaXj56CjmyQLBiadfRCqUbL2VWNbbe1nZU6wiC
			array_bytes::hex2array_unchecked("a71e36636d822d5cc35f0c0da3ce8bd49ce1a1af752ec6d14ca7de3b9ec83497")
				.unchecked_into(),
			// 5EZaeQ8djPcq9pheJUhgerXQZt9YaHnMJpiHMRhwQeinqUW8
			array_bytes::hex2array_unchecked("043f75ff5d2cb4f44a66c88e405b1c2159c2fecb6553b1f47078b50bc89cb702")
				.unchecked_into(),
			// 5EZaeQ8djPcq9pheJUhgerXQZt9YaHnMJpiHMRhwQeinqUW8
			array_bytes::hex2array_unchecked("043f75ff5d2cb4f44a66c88e405b1c2159c2fecb6553b1f47078b50bc89cb702")
				.unchecked_into(),
			// 5EZaeQ8djPcq9pheJUhgerXQZt9YaHnMJpiHMRhwQeinqUW8
			array_bytes::hex2array_unchecked("043f75ff5d2cb4f44a66c88e405b1c2159c2fecb6553b1f47078b50bc89cb702")
				.unchecked_into(),
		),
		(
			// 5CJcVtX9J1gZsUbLwhNgcV8opcwG3nCDtwaTCV15LUVnaB1d
			array_bytes::hex_n_into_unchecked("0a9b65eaf00a8bf6e2de2d5ad1e25b55fe3ea0fe0440585b489f367e8662ae61"),
			// 5CJcVtX9J1gZsUbLwhNgcV8opcwG3nCDtwaTCV15LUVnaB1d
			array_bytes::hex_n_into_unchecked("0a9b65eaf00a8bf6e2de2d5ad1e25b55fe3ea0fe0440585b489f367e8662ae61"),
			// 5EockCXN6YkiNCDjpqqnbcqd4ad35nU4RmA1ikM4YeRN4WcE
			array_bytes::hex2array_unchecked("085d3aa11d114402910c9deef01b7dd706f3d0bdbc6c3bde0077e0ccfac5819b")
				.unchecked_into(),
			// 5DhLtiaQd1L1LU9jaNeeu9HJkP6eyg3BwXA7iNMzKm7qqruQ
			array_bytes::hex2array_unchecked("805132b45c12ae8978a35f548a6b65802bc698b41a94d1ca785fd5f37fc2832c")
				.unchecked_into(),
			// 5DhLtiaQd1L1LU9jaNeeu9HJkP6eyg3BwXA7iNMzKm7qqruQ
			array_bytes::hex2array_unchecked("805132b45c12ae8978a35f548a6b65802bc698b41a94d1ca785fd5f37fc2832c")
				.unchecked_into(),
			// 5DhLtiaQd1L1LU9jaNeeu9HJkP6eyg3BwXA7iNMzKm7qqruQ
			array_bytes::hex2array_unchecked("805132b45c12ae8978a35f548a6b65802bc698b41a94d1ca785fd5f37fc2832c")
				.unchecked_into(),
		),
		(
			// 5HWUGPKUfBaeQa1xv7umvVHa7br6ngCCSBGu6FSUL1QKXfcB
			array_bytes::hex_n_into_unchecked("f0d3cbccf4688386a8c6c2f9b31101dce0f567f5d64770786970e198a28fdc29"),
			// 5HWUGPKUfBaeQa1xv7umvVHa7br6ngCCSBGu6FSUL1QKXfcB
			array_bytes::hex_n_into_unchecked("f0d3cbccf4688386a8c6c2f9b31101dce0f567f5d64770786970e198a28fdc29"),
			// 5E1jLYfLdUQKrFrtqoKgFrRvxM3oQPMbf6DfcsrugZZ5Bn8d
			array_bytes::hex2array_unchecked("da7f37c796073515e0b1c2e90a558ee1ee8da24818baf12eb08fc928b708f13c")
				.unchecked_into(),
			// 5DhKqkHRkndJu8vq7pi2Q5S3DfftWJHGxbEUNH43b46qNspH
			array_bytes::hex2array_unchecked("7876c70485bb894285dd0c6187013d5a8d1f3f0dd46d81e1aa87206c18a7a353")
				.unchecked_into(),
			// 5DhKqkHRkndJu8vq7pi2Q5S3DfftWJHGxbEUNH43b46qNspH
			array_bytes::hex2array_unchecked("7876c70485bb894285dd0c6187013d5a8d1f3f0dd46d81e1aa87206c18a7a353")
				.unchecked_into(),
			// 5DhKqkHRkndJu8vq7pi2Q5S3DfftWJHGxbEUNH43b46qNspH
			array_bytes::hex2array_unchecked("7876c70485bb894285dd0c6187013d5a8d1f3f0dd46d81e1aa87206c18a7a353")
				.unchecked_into(),
		)
	];

	// generated with secret: subkey inspect "$secret"/fir
	let root_key: AccountId = array_bytes::hex_n_into_unchecked(
		// 5GuQdepeV7b2hirjDh9xFmZxkS5ZqacLMnFrRn3THoqJa3wg
		"d615f9e0ffa69a0814852952b614a7c1b9f361454188caca560cca3f1cfaa16f",
	);

	let endowed_accounts: Vec<AccountId> = vec![root_key.clone()];

	testnet_genesis(initial_authorities, vec![], root_key, Some(endowed_accounts))
}

/// Staging testnet config.
pub fn staging_testnet_config() -> ChainSpec {
	let boot_nodes = vec![];
	ChainSpec::from_genesis(
		"Staging Testnet",
		"staging_testnet",
		ChainType::Live,
		staging_testnet_config_genesis,
		boot_nodes,
		Some(
			TelemetryEndpoints::new(vec![(STAGING_TELEMETRY_URL.to_string(), 0)])
				.expect("Staging telemetry url is valid; qed"),
		),
		None,
		None,
		None,
		Default::default(),
	)
}

/// Helper function to generate a crypto pair from seed
pub fn get_from_seed<TPublic: Public>(seed: &str) -> <TPublic::Pair as Pair>::Public {
	TPublic::Pair::from_string(&format!("//{}", seed), None)
		.expect("static values are valid; qed")
		.public()
}

/// Helper function to generate an account ID from seed
pub fn get_account_id_from_seed<TPublic: Public>(seed: &str) -> AccountId
where
	AccountPublic: From<<TPublic::Pair as Pair>::Public>,
{
	AccountPublic::from(get_from_seed::<TPublic>(seed)).into_account()
}

/// Helper function to generate stash, controller and session key from seed
pub fn authority_keys_from_seed(
	seed: &str,
) -> (AccountId, AccountId, GrandpaId, BabeId, ImOnlineId, AuthorityDiscoveryId) {
	(
		get_account_id_from_seed::<sr25519::Public>(&format!("{}//stash", seed)),
		get_account_id_from_seed::<sr25519::Public>(seed),
		get_from_seed::<GrandpaId>(seed),
		get_from_seed::<BabeId>(seed),
		get_from_seed::<ImOnlineId>(seed),
		get_from_seed::<AuthorityDiscoveryId>(seed),
	)
}

/// Helper function to create GenesisConfig for testing
pub fn testnet_genesis(
	initial_authorities: Vec<(
		AccountId,
		AccountId,
		GrandpaId,
		BabeId,
		ImOnlineId,
		AuthorityDiscoveryId,
	)>,
	initial_nominators: Vec<AccountId>,
	root_key: AccountId,
	endowed_accounts: Option<Vec<AccountId>>,
) -> GenesisConfig {
	let mut endowed_accounts: Vec<AccountId> = endowed_accounts.unwrap_or_else(|| {
		vec![
			get_account_id_from_seed::<sr25519::Public>("Alice"),
			get_account_id_from_seed::<sr25519::Public>("Bob"),
			get_account_id_from_seed::<sr25519::Public>("Charlie"),
			get_account_id_from_seed::<sr25519::Public>("Dave"),
			get_account_id_from_seed::<sr25519::Public>("Eve"),
			get_account_id_from_seed::<sr25519::Public>("Ferdie"),
			get_account_id_from_seed::<sr25519::Public>("Alice//stash"),
			get_account_id_from_seed::<sr25519::Public>("Bob//stash"),
			get_account_id_from_seed::<sr25519::Public>("Charlie//stash"),
			get_account_id_from_seed::<sr25519::Public>("Dave//stash"),
			get_account_id_from_seed::<sr25519::Public>("Eve//stash"),
			get_account_id_from_seed::<sr25519::Public>("Ferdie//stash"),
		]
	});
	// endow all authorities and nominators.
	initial_authorities
		.iter()
		.map(|x| &x.0)
		.chain(initial_nominators.iter())
		.for_each(|x| {
			if !endowed_accounts.contains(x) {
				endowed_accounts.push(x.clone())
			}
		});

	// stakers: all validators and nominators.
	let mut rng = rand::thread_rng();
	let stakers = initial_authorities
		.iter()
		.map(|x| (x.0.clone(), x.1.clone(), STASH, StakerStatus::Validator))
		.chain(initial_nominators.iter().map(|x| {
			use rand::{seq::SliceRandom, Rng};
			let limit = (MaxNominations::get() as usize).min(initial_authorities.len());
			let count = rng.gen::<usize>() % limit;
			let nominations = initial_authorities
				.as_slice()
				.choose_multiple(&mut rng, count)
				.into_iter()
				.map(|choice| choice.0.clone())
				.collect::<Vec<_>>();
			(x.clone(), x.clone(), STASH, StakerStatus::Nominator(nominations))
		}))
		.collect::<Vec<_>>();

	let num_endowed_accounts = endowed_accounts.len();

	const ENDOWMENT: Balance = 10_000_000 * DOLLARS;
	const STASH: Balance = ENDOWMENT / 1000;

	GenesisConfig {
		system: SystemConfig { code: wasm_binary_unwrap().to_vec() },
		balances: BalancesConfig {
			balances: endowed_accounts.iter().cloned().map(|x| (x, ENDOWMENT)).collect(),
		},
		indices: IndicesConfig { indices: vec![] },
		session: SessionConfig {
			keys: initial_authorities
				.iter()
				.map(|x| {
					(
						x.0.clone(),
						x.0.clone(),
						session_keys(x.2.clone(), x.3.clone(), x.4.clone(), x.5.clone()),
					)
				})
				.collect::<Vec<_>>(),
		},
		staking: StakingConfig {
			validator_count: initial_authorities.len() as u32,
			minimum_validator_count: initial_authorities.len() as u32,
			invulnerables: initial_authorities.iter().map(|x| x.0.clone()).collect(),
			slash_reward_fraction: Perbill::from_percent(10),
			stakers,
			..Default::default()
		},
		democracy: DemocracyConfig::default(),
		elections: ElectionsConfig {
			members: endowed_accounts
				.iter()
				.take((num_endowed_accounts + 1) / 2)
				.cloned()
				.map(|member| (member, STASH))
				.collect(),
		},
		council: CouncilConfig::default(),
		technical_committee: TechnicalCommitteeConfig {
			members: endowed_accounts
				.iter()
				.take((num_endowed_accounts + 1) / 2)
				.cloned()
				.collect(),
			phantom: Default::default(),
		},
		sudo: SudoConfig { key: Some(root_key) },
		babe: BabeConfig {
			authorities: vec![],
			epoch_config: Some(kitchensink_runtime::BABE_GENESIS_EPOCH_CONFIG),
		},
		im_online: ImOnlineConfig { keys: vec![] },
		authority_discovery: AuthorityDiscoveryConfig { keys: vec![] },
		grandpa: GrandpaConfig { authorities: vec![] },
		technical_membership: Default::default(),
		treasury: Default::default(),
		society: SocietyConfig {
			members: endowed_accounts
				.iter()
				.take((num_endowed_accounts + 1) / 2)
				.cloned()
				.collect(),
			pot: 0,
			max_members: 999,
		},
		vesting: Default::default(),
		assets: Default::default(),
		gilt: Default::default(),
		transaction_storage: Default::default(),
		transaction_payment: Default::default(),
		alliance: Default::default(),
		alliance_motion: Default::default(),
		nomination_pools: NominationPoolsConfig {
			min_create_bond: 10 * DOLLARS,
			min_join_bond: 1 * DOLLARS,
			..Default::default()
		},
		node_authorization: NodeAuthorizationConfig {
    		nodes: vec![
				(
					OpaquePeerId(bs58::decode("12D3KooWEiakrYXZGxYr2UBLwaN4kPPUDsFpwQYy4Eu1VR2c1RLu").into_vec().unwrap()),
					endowed_accounts[0].clone()
				),
				(
					OpaquePeerId(bs58::decode("12D3KooWQ4SmwcPYpeDHYgYC5oux4LiZ3QPpmbuJRY2jfPm4QrxG").into_vec().unwrap()),
					endowed_accounts[1].clone()
				),
				(
					OpaquePeerId(bs58::decode("12D3KooWSjMVmXNEV7Pssyg6Z5KFxREhXqVLd82dcCKqZU7ZWW7b").into_vec().unwrap()),
					endowed_accounts[2].clone()
				),
    		],
   		},

	}
}

fn development_config_genesis() -> GenesisConfig {
	testnet_genesis(
		vec![authority_keys_from_seed("Alice")],
		vec![],
		get_account_id_from_seed::<sr25519::Public>("Alice"),
		None,
	)
}

/// Development config (single validator Alice)
pub fn development_config() -> ChainSpec {
	ChainSpec::from_genesis(
		"Development",
		"dev",
		ChainType::Development,
		development_config_genesis,
		vec![],
		None,
		None,
		None,
		None,
		Default::default(),
	)
}

fn local_testnet_genesis() -> GenesisConfig {
	testnet_genesis(
		vec![authority_keys_from_seed("Alice"), authority_keys_from_seed("Bob")],
		vec![],
		get_account_id_from_seed::<sr25519::Public>("Alice"),
		None,
	)
}

/// Local testnet config (multivalidator Alice + Bob)
pub fn local_testnet_config() -> ChainSpec {
	ChainSpec::from_genesis(
		"Local Testnet",
		"local_testnet",
		ChainType::Local,
		local_testnet_genesis,
		vec![],
		None,
		None,
		None,
		None,
		Default::default(),
	)
}

#[cfg(test)]
pub(crate) mod tests {
	use super::*;
	use crate::service::{new_full_base, NewFullBase};
	use sc_service_test;
	use sp_runtime::BuildStorage;

	fn local_testnet_genesis_instant_single() -> GenesisConfig {
		testnet_genesis(
			vec![authority_keys_from_seed("Alice")],
			vec![],
			get_account_id_from_seed::<sr25519::Public>("Alice"),
			None,
		)
	}

	/// Local testnet config (single validator - Alice)
	pub fn integration_test_config_with_single_authority() -> ChainSpec {
		ChainSpec::from_genesis(
			"Integration Test",
			"test",
			ChainType::Development,
			local_testnet_genesis_instant_single,
			vec![],
			None,
			None,
			None,
			None,
			Default::default(),
		)
	}

	/// Local testnet config (multivalidator Alice + Bob)
	pub fn integration_test_config_with_two_authorities() -> ChainSpec {
		ChainSpec::from_genesis(
			"Integration Test",
			"test",
			ChainType::Development,
			local_testnet_genesis,
			vec![],
			None,
			None,
			None,
			None,
			Default::default(),
		)
	}

	#[test]
	#[ignore]
	fn test_connectivity() {
		sp_tracing::try_init_simple();

		sc_service_test::connectivity(integration_test_config_with_two_authorities(), |config| {
			let NewFullBase { task_manager, client, network, transaction_pool, .. } =
				new_full_base(config, false, |_, _| ())?;
			Ok(sc_service_test::TestNetComponents::new(
				task_manager,
				client,
				network,
				transaction_pool,
			))
		});
	}

	#[test]
	fn test_create_development_chain_spec() {
		development_config().build_storage().unwrap();
	}

	#[test]
	fn test_create_local_testnet_chain_spec() {
		local_testnet_config().build_storage().unwrap();
	}

	#[test]
	fn test_staging_test_net_chain_spec() {
		staging_testnet_config().build_storage().unwrap();
	}
}
