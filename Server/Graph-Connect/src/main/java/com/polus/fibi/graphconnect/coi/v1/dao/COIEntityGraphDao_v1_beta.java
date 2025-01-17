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

import com.polus.fibi.graphconnect.coi.v1.controller.COIGraphController;
import com.polus.fibi.graphconnect.config.Neo4jConnectionManager;
import com.polus.fibi.graphconnect.exceptions.COIEntityGraphProcessingException;
import com.polus.fibi.graphconnect.model.RequestDTO;
import com.polus.fibi.graphconnect.model.ResponseDTO;

@Repository
public class COIEntityGraphDao_v1_beta {

	@Autowired
	Neo4jConnectionManager neo4jConnectionManager;

	@Value("${graphconnect.log.cypher}")
	private String logCypher;

	private static final Logger logger = LoggerFactory.getLogger(COIEntityGraphDao_v1_beta.class);
	
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

			if ("Belongs_to".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Entity)-[r1:BELONGS_TO]->(e2:Country)  where <WHERE_CLAUSE> RETURN p");

			} else if ("Self_Relationship".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Person)-[r1:SELF_RELATIONSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("Spouse_Relationship".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Person)-[r1:SPOUSE_RELATIONSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("Dependant_Relationship".equalsIgnoreCase(relationship)) {
				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Person)-[r1:DEPENDANT_RELATIONSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("citizen_of".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Person)-[r1:CITIZEN_OF]->(e2:Country)  where <WHERE_CLAUSE> RETURN p");

			} else if ("ENTITY_PROPOSAL_RELATIONSHIP".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Entity)-[r1:ENTITY_PROPOSAL_RELATIONSHIP]->(e2:Proposal)  where <WHERE_CLAUSE> RETURN p");

			} else if ("TRAVEL_SPONSORED".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Entity)-[r1:TRAVEL_SPONSORED]->(e2:TravelDisclosure)  where <WHERE_CLAUSE> RETURN p");

			} else if ("MY_TRAVEL_DISCLOSURE".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Person)-[r1:MY_TRAVEL_DISCLOSURE]->(e2:TravelDisclosure)  where <WHERE_CLAUSE> RETURN p");

			} else if ("LINKED_WITH_PROPOSAL".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:COIDisclosure)-[r1:LINKED_WITH_PROPOSAL]->(e2:Proposal)  where <WHERE_CLAUSE> RETURN p");

			} else if ("LINKED_WITH_AWARD".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:COIDisclosure)-[r1:LINKED_WITH_AWARD]->(e2:Award)  where <WHERE_CLAUSE> RETURN p");

			} else if ("FINANCIALLY_SUPPORTED".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Entity)-[r1:FINANCIALLY_SUPPORTED]->(e2:COIDisclosure)  where <WHERE_CLAUSE> RETURN p");

			} else if ("MY_FCOI_DISCLOSURE".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Person)-[r1:MY_FCOI_DISCLOSURE]->(e2:COIDisclosure)  where <WHERE_CLAUSE> RETURN p");

			} else if ("SUBSIDIARY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Entity)-[r1:SUBSIDIARY]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("PARENT".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat("MATCH p=(e1:Entity)-[r1:PARENT]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("ACQUISITION".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Entity)-[r1:ACQUISITION]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("DISTRIBUTORSHIP".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Entity)-[r1:DISTRIBUTORSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("JOINT_VENTURE".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Entity)-[r1:JOINT_VENTURE]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("SIBLINGS_FROM_THE_SAME_PARENT".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Entity)-[r1:SIBLINGS_FROM_THE_SAME_PARENT]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");

			} else if ("PROPOSAL_OWNED_BY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Proposal)-[r1:PROPOSAL_OWNED_BY]->(e2:Unit)  where <WHERE_CLAUSE> RETURN p");

			} else if ("PROPOSAL_PRIME_SPONSORED_BY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Proposal)-[r1:PROPOSAL_PRIME_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");

			} else if ("PROPOSAL_SPONSORED_BY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Proposal)-[r1:PROPOSAL_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");

			} else if ("AWARD_OWNED_BY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher
						.concat("MATCH p=(e1:Award)-[r1:AWARD_OWNED_BY]->(e2:Unit)  where <WHERE_CLAUSE> RETURN p");

			} else if ("AWARD_PRIME_SPONSORED_BY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Award)-[r1:AWARD_PRIME_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");

			} else if ("AWARD_SPONSORED_BY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Award)-[r1:AWARD_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");

			} else if ("AWARD_SPONSORED_BY".equalsIgnoreCase(relationship)) {

				cypher = (cypher != null ? cypher.concat(" UNION ") : "");
				cypher = cypher.concat(
						"MATCH p=(e1:Entity)-[r1:ENTITY_AWARD_RELATIONSHIP]->(e2:Award)  where <WHERE_CLAUSE> RETURN p");

			}

		}

		if ("Entity".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.entity_number = $param1 OR e2.entity_number = $param1");

		} else if ("Person".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.person_id = $param1");

		} else if ("COIDisclosure".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.disclosure_number = $param1");

		} else if ("Proposal".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.proposal_id = $param1");

		} else if ("Award".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.award_number = $param1");

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
