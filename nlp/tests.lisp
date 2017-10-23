;;;***************************************************************************
;;;* Copyright 2017 by Cancer Commons                                        *
;;;*                                                                         *
;;;* Permission is hereby granted, free of charge, to any person obtaining   *
;;;* a copy of this software and associated documentation files (the         *
;;;* "Software"), to deal in the Software without restriction, including     *
;;;* without limitation the rights to use, copy, modify, merge, publish,     *
;;;* distribute, sublicense, and/or sell copies of the Software, and to      *
;;;* permit persons to whom the Software is furnished to do so, subject to   *
;;;* the following conditions:                                               *
;;;*                                                                         *
;;;* The above copyright notice and this permission notice shall be          *
;;;* included in all copies or substantial portions of the Software.         *
;;;*                                                                         *
;;;* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         *
;;;* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      *
;;;* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                   *
;;;* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE  *
;;;* LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION  *
;;;* OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION   *
;;;* WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.         *
;;;***************************************************************************


("EGFR expression confers sensitivity to temozolomide in GBM."
"
      {\"cancer\":[\"CANCERTYPE.GBM\"],
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TEMOZOLOMIDE\"]
      }
      ")

("In GBM, EGFR expression confers sensitivity to temozolomide."
"
      {\"cancer\":[\"CANCERTYPE.GBM\"],
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TEMOZOLOMIDE\"]
      }
      ")

("In a randomized controlled trial involving 100 patients with stage 2 metastatic lung cancer (shrager, 1998), EGFR expression confers sensitivity to dexamethasone."
"
      {\"cancer\":[\"stage\",2,\"metastatic\",\"CANCERTYPE.LUNG\",\"cancer\"],
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":\"shrager, 1998\",
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":\"rct\",
      \"cases\":100,
      \"drug\":[\"DEXAMETHASONE\"]
      }
      ")

("EGFR expression shows resistance to temozolomide in this elderly man."
"
      {\"cancer\":null,
      \"relation\":\"resistance_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":\"case_study\",
      \"cases\":[{\"age\":\"Elderly\",\"gender\":\"MALE\"}],
      \"drug\":[\"TEMOZOLOMIDE\"]
      }
      ")

("NRAS expression conferred resistance to ipilimumab in a 50 year old man with lung cancer."
"
      {\"cancer\":[\"CANCERTYPE.LUNG\",\"cancer\"],
      \"relation\":\"resistance_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.NRAS\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":null,
      \"cases\":[{\"age\":50,\"gender\":\"MALE\"}],
      \"drug\":[\"IPILIMUMAB\"]
      }
      ")

("EGFR expression confers sensitivity to paclitaxel in metastatic breast cancer."
"
      {\"cancer\":[\"metastatic\",\"CANCERTYPE.BREAST\",\"cancer\"],
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"PACLITAXEL\"]
      }
      ")

("In a randomized controlled trial with 200 patients with stage 2 metastatic melanoma, EGFR expression leads to resistance to cisplatin."
"
      {\"cancer\":[\"stage\",2,\"metastatic\",\"CANCERTYPE.MELANOMA\"],
      \"relation\":\"resistance_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":\"rct\",
      \"cases\":200,
      \"drug\":[\"CISPLATIN\"]
      }
      ")

("Keppra induces fatigue and wobbliness"
"
      {\"cancer\":null,
      \"relation\":[\"INDUCES\"],
      \"adverse-event\":[[\"AE.FATIGUE\",\"and\",\"AE.WOBBLINESS\"]],
      \"reference\":null,
      \"molecule\":[],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"KEPPRA\"]
      }
      ")

("TMZ lead to toxicity in a clinical trial of 30 patients with GBM."
"
      {\"cancer\":[\"CANCERTYPE.GBM\"],
      \"relation\":[\"LEAD\"],
      \"adverse-event\":[[\"AE.TOXICITY\"]],
      \"reference\":null,
      \"molecule\":[],
      \"model\":\"trial\",
      \"cases\":30,
      \"drug\":[\"TMZ\"]
      }
      ")

("Sorafenib significantly accelerated disease progression in a phase II clinical trial of 11 patients with PLGA."
"
      {\"cancer\":[\"CANCERTYPE.PLGA\"],
      \"relation\":[\"ACCELERATED\"],
      \"adverse-event\":[[\"AE.DISEASE\",\"AE.PROGRESSION\"]],
      \"reference\":null,
      \"molecule\":[],
      \"model\":\"trial\",
      \"cases\":11,
      \"drug\":[\"SORAFENIB\"]
      }
      ")

("EGFR expression shows resistance to temozolomide in this elderly man."
"
      {\"cancer\":null,
      \"relation\":\"resistance_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":\"case_study\",
      \"cases\":[{\"age\":\"elderly\",\"gender\":\"MALE\"}],
      \"drug\":[\"TEMOZOLOMIDE\"]
      }
      ")

("EGFR deletion confers resistance to cisplatin in lung cancer."
"
      {\"cancer\":[\"CANCERTYPE.LUNG\",\"cancer\"],
      \"relation\":\"resistance_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.DELETION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"CISPLATIN\"]
      }
      ")

("Radiotherapy caused major toxicity in a trial of 117 patients with brain cancer."
"
      {\"cancer\":[\"CANCERTYPE.BRAIN\",\"cancer\"],
      \"relation\":[\"CAUSED\"],
      \"adverse-event\":[[\"AE.MAJOR\",\"AE.TOXICITY\"]],
      \"reference\":null,
      \"molecule\":[],
      \"model\":\"trial\",
      \"cases\":117,
      \"drug\":[\"RADIOTHERAPY\"]
      }
      ")

("In lung cancer, egfr deletion confers sensitivity tmz and cisplatin."
"
      {\"cancer\":[\"CANCERTYPE.LUNG\",\"cancer\"],
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.DELETION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TMZ\",\"CISPLATIN\"]
      }
      ")

("EGFR deletion confers sensitivity tmz and cisplatin in lung cancer."
"
      {\"cancer\":[\"CANCERTYPE.LUNG\",\"cancer\"],
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.DELETION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TMZ\",\"CISPLATIN\"]
      }
      ")

("EGFR deletion confers sensitivity tmz and cisplatin."
"
      {\"cancer\":null,
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.DELETION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TMZ\",\"CISPLATIN\"]
      }
      ")

("EGFR expression confers sensitivity tmz and cisplatin."
"
      {\"cancer\":null,
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TMZ\",\"CISPLATIN\"]
      }
      ")

("BRAF expression confers sensitivity tmz and cisplatin."
"
      {\"cancer\":null,
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.BRAF\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TMZ\",\"CISPLATIN\"]
      }
      ")

("EGFR deletion and braf expression confers sensitivity tmz and cisplatin."
"
      {\"cancer\":null,
      \"relation\":\"sensitivity_to\",
      \"adverse-event\":null,
      \"reference\":null,
      \"molecule\":[{\"name\":\"M.EGFR\",\"state\":\"MUTYPEWORD.DELETION\"},\"{\"name\":\"M.BRAF\",\"state\":\"MUTYPEWORD.EXPRESSION\"}],
      \"model\":null,
      \"cases\":\"undefined\",
      \"drug\":[\"TMZ\",\"CISPLATIN\"]
      }
      ")

("In a ranomized controled trial invoving 100 patents with stage 2 metstatic lugn cacner (shrager, 1998), EGFR experssion confers sensiitvity to dexmaethsaon."
"[\"_PARSEFAILED_PROPOSED_SPELLING_CORRECTIONS_\",{\"ranomized\":\"randomized\",\"controled\":\"controlled\",\"invoving\":\"involving\",\"patents\":\"patients\",\"metstatic\":\"metastatic\",\"lugn\":\"slug\",\"cacner\":\"dacarbazine-+-quercetin\",\"experssion\":\"expression\",\"sensiitvity\":\"sensitivity\",\"dexmaethsaon\":\"dexamethasone\"}]")
