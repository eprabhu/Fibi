package com.polus.fibi.graphconnect.coi.v1.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.stream.Stream;

import org.neo4j.driver.Driver;
import org.neo4j.driver.Record;
import org.neo4j.driver.Result;
import org.neo4j.driver.Session;
import org.neo4j.driver.types.Node;
import org.neo4j.driver.types.Path;
import org.neo4j.driver.types.Relationship;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.config.Neo4jConnectionManager;
import com.polus.fibi.graphconnect.exceptions.COIEntityGraphProcessingException;
import com.polus.fibi.graphconnect.model.RequestDTO;
import com.polus.fibi.graphconnect.model.ResponseDTO;

@Repository
public class COIEntityGraphDao {

	@Autowired
	Neo4jConnectionManager neo4jConnectionManager;

	@Value("${graphconnect.log.cypher}")
	private String logCypher;

	private static final Logger logger = LoggerFactory.getLogger(COIEntityGraphDao.class);

	private static final String CYPHER_ALIAS = "p";

	private static final String ELEMENT_ID = "elementId";

	private static final String LABEL = "label";

	private static final String SOURCE = "source";

	private static final String TARGET = "target";

	private static final String TYPE = "type";

	private static final String PARAM_1 = "param1";

	public ResponseDTO entityGraphDAO(RequestDTO request) {

		ArrayList<Map<String, Object>> outputNode = new ArrayList<>();
		ArrayList<Map<String, Object>> outputRel = new ArrayList<>();

		try {
			Driver driver = neo4jConnectionManager.getDriver();

			try (Session session = driver.session()) {

				String cypher = getCypher(request);

				logQuery(cypher);

				Map<String, Object> parameters = new HashMap<>();
				parameters.put(PARAM_1, request.getValue());

				Result result = session.run(cypher, parameters);

				HashSet<String> nodeElementSet = new HashSet<String>();
				while (result.hasNext()) {
					Record record = result.next();

					// Retrieve the path from the record
					Path path = record.get(CYPHER_ALIAS).asPath();

					// Retrieve nodes and relationships from the path
					Iterable<Node> nodes = path.nodes();
					Iterable<Relationship> relationships = path.relationships();

					// Process nodes
					for (Node node : nodes) {

						String elementId = node.elementId();

						if (!nodeElementSet.contains(elementId)) {
							Map<String, Object> hmNode = new HashMap<>();
							hmNode.put(ELEMENT_ID, elementId);

							Stream<String> stream = Stream.generate(node.labels().iterator()::next);
							// String firstElement = stream.findFirst().orElse("-no data-");

							hmNode.put(LABEL, stream.findFirst().orElse("-no data-"));

							// hmNode.put("label", node.labels().toString());
							nodeElementSet.add(node.elementId());

							// Iterate over entries (keys and values)
							for (Map.Entry<String, Object> entry : node.asMap().entrySet()) {
								hmNode.put(entry.getKey(), entry.getValue());
							}
							outputNode.add(hmNode);
						}

					}

					// Process relationships
					for (Relationship relationship : relationships) {
						Map<String, Object> hmRel = new HashMap<>();
						hmRel.put(SOURCE, relationship.startNodeElementId());
						hmRel.put(TARGET, relationship.endNodeElementId());
						hmRel.put(TYPE, relationship.type());

						for (Map.Entry<String, Object> entry : relationship.asMap().entrySet()) {
							hmRel.put(entry.getKey(), entry.getValue());
						}

						outputRel.add(hmRel);

					}
				}
			}

		} catch (RuntimeException e) {

			throw new COIEntityGraphProcessingException("Runtime exception " + e.getMessage()
					+ ", COI Entity Graph Processing Exception for the request --> " + request.toString());

		}

		return ResponseDTO.builder().nodes(outputNode).links(outputRel).build();

	}

	private String getCypher(RequestDTO request) {

		String cypher = null;
		// boolean
		ArrayList<String> lsRelationships = request.getRelationship();

		for (String relationship : lsRelationships) {

			if ("BELONGS_TO".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Entity)-[r1:BELONGS_TO]->(e2:Country)  where <WHERE_CLAUSE> RETURN p");

			} else if ("ASSOCIATED_ENTITIES".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Entity)-[r1:ASSOCIATED_ENTITIES]->(e2:Person)  where <WHERE_CLAUSE> RETURN p");

			} else if ("CITIZEN_OF".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Person)-[r1:CITIZEN_OF]->(e2:Country)  where <WHERE_CLAUSE> RETURN p");

			} else if ("AFFILIATED_ENTITIES".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						" MATCH p=(c1:Country)<--(e2:Entity)-[r1:AFFILIATED_ENTITIES]->(e1:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("ASSOCIATED_PERSONS".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Person)-[r1:ASSOCIATED_ENTITIES]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
						

			} else if ("ENTITIES_BELONGS_WITH".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Country)-[r1:BELONGS_TO]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("ALL_CITIZENS".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Country)-[r1:CITIZEN_OF]->(e2:Person)  where <WHERE_CLAUSE> RETURN p");

			}else if ("ASSOCIATED_PERSONS_WITH_CITIZEN_INFO".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				String gql = """
						
						MATCH p= (e1:Person)-[r1:ASSOCIATED_ENTITIES]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p 

						UNION
						
						MATCH p= (c1:Country)<-[r0:CITIZEN_OF]-(e1:Person)-[r1:ASSOCIATED_ENTITIES]->(e2:Entity) where <WHERE_CLAUSE> RETURN p 

						""";
				cypher = cypher.concat(gql);

			}
			
			

		}

		if ("Entity".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.entity_number = $param1 OR e2.entity_number = $param1");

		} else if ("Person".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.person_id = $param1");

		} else if ("Country".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.country_code = $param1");

		}

		return cypher;
	}

	private void logQuery(String cypher) {
		if ("Y".equalsIgnoreCase(logCypher)) {
			String message = "\n=============================== LOG CYPHER =======================================\n";
			logger.info(message.concat(cypher).concat(message));
		}
	}

}
