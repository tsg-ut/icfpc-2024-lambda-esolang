import { createApp, ref } from 'https://unpkg.com/vue@3/dist/vue.esm-browser.js'
import { Simulator } from './3d-simulator.mjs'

createApp({
  data() {
    return {
      code: '',
      inputA: 0,
      inputB: 0,
      output: 'N/A',
      hoge: 100,
      errorMessage: '',
      history: [],
      tick: 0,
    };
  },
  setup() {
    const message = ref('Hello Vue!');
    return {
      message,
    };
  },
  methods: {
    run() {
      this.errorMessage = '';
      this.output = 'N/A';

      let simulator;
      try {
        simulator = new Simulator(this.code, this.inputA, this.inputB);
      } catch (e) {
        this.errorMessage = `Initialization error: ${e}`;
        return;
      }

      try {
        const result = simulator.run();
        this.output = result;
      } catch (e) {
        this.errorMessage = `Exited with error: ${e}`;
      }

      this.history = simulator.history;
      this.tick = 0;
    },
    backTick(n) {
      this.tick = Math.max(0, this.tick - n);
    },
    forwardTick(n) {
      this.tick = Math.min(this.history.length - 1, this.tick + n);
    },
  },
  computed: {
    board() {
      return this.history[this.tick];
    },
  },
}).mount('#app')