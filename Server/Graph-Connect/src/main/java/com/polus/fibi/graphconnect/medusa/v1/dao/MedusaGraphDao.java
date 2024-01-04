package com.polus.fibi.graphconnect.medusa.v1.dao;

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
public class MedusaGraphDao {

	@Autowired
	Neo4jConnectionManager neo4jConnectionManager;

	@Value("${graphconnect.log.cypher}")
	private String logCypher;

	private static final Logger logger = LoggerFactory.getLogger(MedusaGraphDao.class);

	private static final String NODE_GRANT_CALL = "GrantCall";
	
	private static final String NODE_PROPOSAL = "Proposal";

	private static final String NODE_SERVICE_REQUEST = "ServiceRequest";

	private static final String NODE_AGREEMENT = "Agreement";

	private static final String NODE_AWARD = "Award";

	private static final String NODE_INST_PROPOSAL = "InstituteProposal";

	private static final String PARAM_1 = "param1";

	private static final String RETURN_NODE = "RETURN p";

	private static final String CYPHER_ALIAS = "p";

	private static final String ELEMENT_ID = "elementId";

	private static final String LABEL = "label";

	private static final String SOURCE = "source";

	private static final String TARGET = "target";

	private static final String TYPE = "type";

	public ResponseDTO getMedusaGraph(RequestDTO request) {

		ArrayList<Map<String, Object>> outputNode = new ArrayList<>();
		ArrayList<Map<String, Object>> outputRel = new ArrayList<>();

		try {
			Driver driver = neo4jConnectionManager.getDriver();
			try (Session session = driver.session()) {
				String cypher = getCypher(request);
				if (cypher != null) {
					logQuery(cypher);
					Map<String, Object> parameters = new HashMap<>();
					parameters.put(PARAM_1, request.getValue());
					Result result = session.run(cypher, parameters);
					HashSet<String> nodeElementSet = new HashSet<String>();
					while (result.hasNext()) {
						Record record = result.next();
						Path path = record.get(CYPHER_ALIAS).asPath();
						Iterable<Node> nodes = path.nodes();
						Iterable<Relationship> relationships = path.relationships();
						for (Node node : nodes) {
							String elementId = node.elementId();
							if (!nodeElementSet.contains(elementId)) {
								Map<String, Object> hmNode = new HashMap<>();
								hmNode.put(ELEMENT_ID, elementId);
								Stream<String> stream = Stream.generate(node.labels().iterator()::next);
								hmNode.put(LABEL, stream.findFirst().orElse("-no data-"));
								nodeElementSet.add(node.elementId());
								for (Map.Entry<String, Object> entry : node.asMap().entrySet()) {
									hmNode.put(entry.getKey(), entry.getValue());
								}
								outputNode.add(hmNode);
							}
						}
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
			}
		} catch (RuntimeException e) {

			throw new COIEntityGraphProcessingException("Runtime exception " + e.getMessage()
					+ ", Medusa Graph Processing Exception for the request --> " + request.toString());
		}
		return ResponseDTO.builder().nodes(outputNode).links(outputRel).build();
	}

	private String getCypher(RequestDTO request) {
			HashMap<String,HashMap<String,String>> nodeAliasMap = new HashMap<>();
			String cypher = null;
			ArrayList<String> lsRelationships = request.getRelationship();
			String node = request.getNode();
			String findBy = request.getFindBy();
			for (String relationship : lsRelationships) {
				if ("INITIATES".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					cypher = cypher
							.concat("MATCH p=(e1:GrantCall)-[r1:INITIATES]->(e2:Proposal) ");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_GRANT_CALL, "e1");	
					aliasHm.put(NODE_PROPOSAL, "e2");	
					nodeAliasMap.put(relationship, aliasHm);
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					cypher = cypher
								.concat(whereClause)
								.concat(RETURN_NODE);
				} else if ("RESULTS_IN".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					cypher = cypher.concat(
							"MATCH p=(e1:GrantCall)-[r1:RESULTS_IN]->(e2:Award) ");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_GRANT_CALL, "e1");	
					aliasHm.put(NODE_AWARD, "e2");	
					nodeAliasMap.put(relationship, aliasHm);
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					cypher = cypher
								.concat(whereClause)
								.concat(RETURN_NODE);
				} else if ("APPROVED_AS".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					cypher = cypher
							.concat("MATCH p=(e1:Proposal)-[r1:APPROVED_AS]->(e2:InstituteProposal) ");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_PROPOSAL, "e1");	
					aliasHm.put(NODE_INST_PROPOSAL, "e2");	
					nodeAliasMap.put(relationship, aliasHm);
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					cypher = cypher
								.concat(whereClause)
								.concat(RETURN_NODE);
				} else if ("FUNDED_BY".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					cypher = cypher
							.concat("MATCH p=(e1:InstituteProposal)-[r1:FUNDED_BY]->(e2:Award)");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_INST_PROPOSAL, "e1");	
					aliasHm.put(NODE_AWARD, "e2");	
					nodeAliasMap.put(relationship, aliasHm);
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					cypher = cypher
								.concat(whereClause)
								.concat(RETURN_NODE);
				} else if ("ASSOCIATED_WITH".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					HashMap<String, String> aliasHm = new HashMap<>();
					String whereClause;
					switch (node) {
					case NODE_AWARD:
						cypher = cypher.concat("MATCH p=(e1:Award)-[r1:ASSOCIATED_WITH]->(e2:Agreement)");
						aliasHm.put(NODE_AWARD, "e1");
						aliasHm.put(NODE_AGREEMENT, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_PROPOSAL:
						cypher = cypher.concat("MATCH p=(e1:Proposal)-[r1:ASSOCIATED_WITH]->(e2:Agreement) ");
						aliasHm.put(NODE_PROPOSAL, "e1");
						aliasHm.put(NODE_AGREEMENT, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_INST_PROPOSAL:
						cypher = cypher.concat("MATCH p=(e1:InstituteProposal)-[r1:ASSOCIATED_WITH]->(e2:Agreement)");
						aliasHm.put(NODE_INST_PROPOSAL, "e1");
						aliasHm.put(NODE_AGREEMENT, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_AGREEMENT:
						cypher = cypher.concat("MATCH p=()-[r:ASSOCIATED_WITH]->() WHERE any(node in nodes(p)");
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					default:
						break;
					}
				} else if ("RELATES_TO".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					HashMap<String, String> aliasHm = new HashMap<>();
					String whereClause;
					switch (node) {
					case NODE_AWARD:
						cypher = cypher.concat("MATCH p=(e2:ServiceRequest)-[r1:RELATES_TO]->(e1:Award)");
						aliasHm.put(NODE_AWARD, "e1");
						aliasHm.put(NODE_SERVICE_REQUEST, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_PROPOSAL:
						cypher = cypher.concat("MATCH p=(e2:ServiceRequest)-[r1:RELATES_TO]->(e1:Proposal) ");
						aliasHm.put(NODE_PROPOSAL, "e1");
						aliasHm.put(NODE_SERVICE_REQUEST, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_INST_PROPOSAL:
						cypher = cypher.concat("MATCH p=(e2:ServiceRequest)-[r1:RELATES_TO]->(e1:InstituteProposal)");
						aliasHm.put(NODE_INST_PROPOSAL, "e1");
						aliasHm.put(NODE_SERVICE_REQUEST, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_GRANT_CALL:
						cypher = cypher.concat("MATCH p=(e1:ServiceRequest)-[r1:RELATES_TO]->(e2:GrantCall) ");
						aliasHm.put(NODE_SERVICE_REQUEST, "e1");
						aliasHm.put(NODE_GRANT_CALL, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_SERVICE_REQUEST:
						cypher = cypher.concat("MATCH p=()-[r:RELATES_TO]->() WHERE any(node in nodes(p)");
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					case NODE_AGREEMENT:
						cypher = cypher.concat("MATCH p=(e2:ServiceRequest)-[r1:RELATES_TO]->(e1:Agreement)");
						aliasHm.put(NODE_AGREEMENT, "e1");
						aliasHm.put(NODE_SERVICE_REQUEST, "e2");
						nodeAliasMap.put(relationship, aliasHm);
						whereClause = buildWhereClause(relationship, node, nodeAliasMap, findBy);
						cypher = cypher.concat(whereClause).concat(RETURN_NODE);
						break;
					default:
						break;
					}
				} else if ("AGREEMENT_TO_AGREEMENT".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					cypher = cypher
							.concat("MATCH p=(e1:Agreement)-[r1:AGREEMENT_TO_AGREEMENT]->(e2:Agreement)");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_AGREEMENT, "e1");	
					nodeAliasMap.put(relationship, aliasHm);
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					cypher = cypher
								.concat(whereClause)
								.concat(RETURN_NODE);
				} else if ("SR_TO_SR".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					cypher = cypher
							.concat("MATCH p=(e1:ServiceRequest)-[r1:SR_TO_SR]->(e2:ServiceRequest)");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_SERVICE_REQUEST, "e1");	
					nodeAliasMap.put(relationship, aliasHm);
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					cypher = cypher
								.concat(whereClause)
								.concat(RETURN_NODE);
				}else if ("AWARD_CHAIN_INFO".equalsIgnoreCase(relationship)) {

					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_AWARD, "a1");	
					aliasHm.put(NODE_INST_PROPOSAL, "i1");
					aliasHm.put(NODE_PROPOSAL, "p1");	
					aliasHm.put(NODE_GRANT_CALL, "g1");	
					nodeAliasMap.put(relationship, aliasHm);	
					String gq2 = "MATCH p= (a1:Award)<-[r0:FUNDED_BY]-(i1:InstituteProposal)<-[r1:APPROVED_AS]-(p1:Proposal)<-[r2:INITIATES]-(g1:GrantCall) ";
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					gq2 = gq2
							.concat(whereClause)
							.concat(RETURN_NODE);
					cypher = cypher.concat(gq2);
				}else if ("CHILD_AWARDS".equalsIgnoreCase(relationship)) {
					cypher = (cypher != null ? cypher.concat(" UNION ") : "");
					cypher = cypher
							.concat("MATCH p=(a1:Award)-[r1:CHILD_AWARDS]->(a2:Award)");
					HashMap<String,String> aliasHm = new HashMap<>();
					aliasHm.put(NODE_AWARD, "a1");	
					nodeAliasMap.put(relationship, aliasHm);
					String whereClause = buildWhereClause(relationship,node,nodeAliasMap, findBy);
					cypher = cypher
								.concat(whereClause)
								.concat(RETURN_NODE);
				}
			}
			return cypher;
		}

	private String buildWhereClause(String relationship, String node, HashMap<String, HashMap<String, String>> nodeAliasMap, String findBy) {
		String whereClause = "";
		HashMap<String, String> aliasMap = nodeAliasMap.get(relationship);
		if (NODE_GRANT_CALL.equalsIgnoreCase(node)) {				
			String nodeAliasValue = aliasMap.get(NODE_GRANT_CALL);				
			whereClause = nodeAliasValue + "." + "grant_call_id = $param1 ";
		} else if (NODE_PROPOSAL.equalsIgnoreCase(node)) {
			String nodeAliasValue = aliasMap.get(NODE_PROPOSAL);				
			whereClause = nodeAliasValue + "." + "proposal_id = $param1 ";
		} else if (NODE_INST_PROPOSAL.equalsIgnoreCase(node)) {
			String nodeAliasValue = aliasMap.get(NODE_INST_PROPOSAL);				
			whereClause = nodeAliasValue + "." + "proposal_number = $param1 ";				
		} else if (NODE_AWARD.equalsIgnoreCase(node)) {
			String nodeAliasValue =  aliasMap.get(NODE_AWARD);				
			whereClause = nodeAliasValue + "." + "award_number = $param1 ";				
		} else if (NODE_AGREEMENT.equalsIgnoreCase(node)) {
			whereClause = "node."  + "agreement_request_id = $param1) ";				
		} else if (NODE_SERVICE_REQUEST.equalsIgnoreCase(node)) {
			whereClause = "node."  + "service_request_id = $param1) ";				
		}
		return " WHERE "+ whereClause;
	}

	private void logQuery(String cypher) {
		if ("Y".equalsIgnoreCase(logCypher)) {
			String message = "\n=============================== LOG CYPHER =======================================\n";
			logger.info(message.concat(cypher).concat(message));
		}
	}

}
