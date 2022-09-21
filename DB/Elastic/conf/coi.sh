
curl -XDELETE http://192.168.1.47:9200/fibicoi/

curl -XPUT 'http://192.168.1.47:9200/fibicoi/' -H 'Content-Type: application/json' -d '{"settings":{"number_of_shards":5,"analysis":{"filter":{"ngram_filter":{"type":"edgeNGram","min_gram":2,"max_gram":20}},"analyzer":{"ngram_analyzer":{"type":"custom","tokenizer":"whitespace","filter":["lowercase","ngram_filter"]}}}},"mappings":{"coi":{"_all":{"type":"text","analyzer":"ngram_analyzer","search_analyzer":"whitespace"},"properties":{"disclosure_id":{"type":"text","analyzer":"ngram_analyzer","search_analyzer":"standard"},"category":{"type":"text","analyzer":"ngram_analyzer","search_analyzer":"standard"},"full_name":{"type":"text","analyzer":"ngram_analyzer","search_analyzer":"standard"},"status":{"type":"text","analyzer":"ngram_analyzer","search_analyzer":"keyword"},"person_id":{"type":"text","analyzer":"ngram_analyzer","search_analyzer":"standard"}}}}}'


