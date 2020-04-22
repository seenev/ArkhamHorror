import { GetterTree, ActionTree, MutationTree } from 'vuex';
import {
  RootState,
} from '@/types';
import {
  ArkhamGameState,
  ArkhamGame,
  ArkhamCycle,
} from '@/arkham/types';
import api from '@/api';

const mutations: MutationTree<ArkhamGameState> = {
  setCycles(state, cycles) {
    state.cycles = cycles;
  },
  setScenarios(state, scenarios) {
    state.scenarios = scenarios;
  },
  setGame(state: ArkhamGameState, game: ArkhamGame) {
    state.game = game;
  },
};

const actions: ActionTree<ArkhamGameState, RootState> = {
  fetchCycles({ state, commit }): Promise<void> {
    if (state.cycles.length === 0) {
      return api.get<string[]>('arkham/cycles').then((cycles) => {
        commit('setCycles', cycles.data);
      });
    }

    return Promise.resolve();
  },

  fetchScenarios({ state, commit }): Promise<void> {
    if (Object.keys(state.scenarios).length === 0) {
      return api.get<string[]>('arkham/scenarios').then((scenarios) => {
        commit('setScenarios', scenarios.data);
      });
    }

    return Promise.resolve();
  },

  startGame({ commit }, { cycle, scenario }): Promise<void> {
    return api.post<string>('arkham/games', { cycleId: cycle.id, scenarioId: scenario.id }).then((game) => {
      commit('setGame', game.data);
    });
  },

  startCampaign({ commit }, { cycle, difficulty, deckUrl }): Promise<ArkhamGame> {
    return api.post<ArkhamGame>('arkham/campaigns', { cycleId: cycle.id, difficulty, deckUrl }).then((game) => {
      commit('setGame', game.data);
      return Promise.resolve(game.data);
    });
  },
};

const getters: GetterTree<ArkhamGameState, RootState> = {
  cycles: (state) => state.cycles,
  cycleScenarios: (state) => (cycle: ArkhamCycle) => state.scenarios[cycle.id],
};

const state: ArkhamGameState = {
  cycles: [],
  scenarios: {},
  game: null,
};

const store = {
  state,
  getters,
  actions,
  mutations,
};

export default store;
