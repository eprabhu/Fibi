package com.polus.appcoigraph.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.Record;
import org.neo4j.driver.Result;
import org.neo4j.driver.Session;
import org.neo4j.driver.internal.value.NodeValue;
import org.neo4j.driver.internal.value.RelationshipValue;
import org.neo4j.driver.types.Node;
import org.neo4j.driver.types.Path;
import org.neo4j.driver.types.Relationship;
import org.springframework.stereotype.Repository;
import org.springframework.stereotype.Service;

import com.polus.appcoigraph.model.RequestDTO;
import com.polus.appcoigraph.model.ResponseDTO;

@Repository  
public class COIEntityGraphDao {

	
	
	
	public ResponseDTO entityGraphDAO(RequestDTO request) {
    	// Create a Neo4j driver
    	Driver driver = GraphDatabase.driver("neo4j://192.168.1.248:7687", AuthTokens.basic("neo4j", "Polus@123"));

    	// Create a session
    		////	Session session = driver.session();
    	// Execute the query
    	///List<Record> results = session.run("MATCH p=(e1:Entity)<--(e2:Person) -->(e3:Country) where e1.entity_number='17' RETURN e1 , e2 , e3").list();

    	// Close the session
    	////session.close();

    	List<Node> cl = new ArrayList<>();
    	List<RelationshipValue> rl = new ArrayList<>();
    	List<NodeValue> el = new ArrayList<>();
    	
    	ArrayList<Map<String, Object>> outputNode = new ArrayList<>();
    	ArrayList<Map<String, Object>> outputRel = new ArrayList<>();
    	
        try (Session session = driver.session()) {
        	
        //  String cypher = "MATCH p = (c:Country)<-[]->(e:Entity) WHERE c.country_code = 'USA'  RETURN p ";
        	
        	String cypher = getCypher(request);
 
        	Map<String, Object> parameters = new HashMap<>();
            parameters.put("param1", request.getValue());

        	
            Result result = session.run(cypher,parameters);
           
            HashSet<String> nodeElementSet = new HashSet<String>();
            while (result.hasNext()) {
               Record record = result.next();
            	
            
            	 // Retrieve the path from the record
                Path path = record.get("p").asPath();

                // Retrieve nodes and relationships from the path
                Iterable<Node> nodes = path.nodes();
                Iterable<Relationship> relationships = path.relationships();

                
                
               
             // Process nodes
                for (Node node : nodes) {       
                  
                	String elementId = node.elementId();
                	
                	if(!nodeElementSet.contains(elementId)) {
	                	Map<String, Object> hmNode = new HashMap<>();
	                    hmNode.put("elementId",elementId );
	                    
	                    Stream<String> stream = Stream.generate(node.labels().iterator()::next);
	                   // String firstElement = stream.findFirst().orElse("-no data-");
	                    
	                    hmNode.put("label", stream.findFirst().orElse("-no data-"));
	                    
	                   // hmNode.put("label", node.labels().toString());
	                    nodeElementSet.add(node.elementId());
	                    
	                 // Iterate over entries (keys and values)
	                    for (Map.Entry<String, Object> entry :  node.asMap().entrySet()) {	                        
	                        hmNode.put(entry.getKey(), entry.getValue());	                        
	                    }
	                    outputNode.add(hmNode);
                	}
                	

                }

                // Process relationships
                for (Relationship relationship : relationships) {
                	Map<String, Object> hmRel = new HashMap<>();
                    hmRel.put("source", relationship.startNodeElementId());
                    hmRel.put("target", relationship.endNodeElementId());
                    hmRel.put("type", relationship.type());
                    
                    for (Map.Entry<String, Object> entry :  relationship.asMap().entrySet()) {                        
                    	hmRel.put(entry.getKey(), entry.getValue());                        
                    }
                   
                    outputRel.add(hmRel);
                    
                }
            }
        }
        
        return ResponseDTO.builder()
        				  .nodes(outputNode)
        				  .links(outputRel)
        				  .build();

    }

	private String getCypher(RequestDTO request) {

		String cypher = null;
		
		

		
		ArrayList<String> lsRelationships = request.getRelationship();
				
			
		for(String relationship : lsRelationships) {
			
			cypher = (cypher != null ? cypher.concat(" UNION "): "");
			
			if("Belongs_to".equalsIgnoreCase(relationship)) {
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:BELONGS_TO]->(e2:Country)  where <WHERE_CLAUSE> RETURN p");
			
			}else if("Self_Relationship".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Person)-[r1:SELF_RELATIONSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("Spouse_Relationship".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Person)-[r1:SPOUSE_RELATIONSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("Dependant_Relationship".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Person)-[r1:DEPENDANT_RELATIONSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("citizen_of".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Person)-[r1:CITIZEN_OF]->(e2:Country)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("ENTITY_PROPOSAL_RELATIONSHIP".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:ENTITY_PROPOSAL_RELATIONSHIP]->(e2:Proposal)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("TRAVEL_SPONSORED".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:TRAVEL_SPONSORED]->(e2:TravelDisclosure)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("MY_TRAVEL_DISCLOSURE".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Person)-[r1:MY_TRAVEL_DISCLOSURE]->(e2:TravelDisclosure)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("LINKED_WITH_PROPOSAL".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:COIDisclosure)-[r1:LINKED_WITH_PROPOSAL]->(e2:Proposal)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("LINKED_WITH_AWARD".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:COIDisclosure)-[r1:LINKED_WITH_AWARD]->(e2:Award)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("FINANCIALLY_SUPPORTED".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:FINANCIALLY_SUPPORTED]->(e2:COIDisclosure)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("MY_FCOI_DISCLOSURE".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Person)-[r1:MY_FCOI_DISCLOSURE]->(e2:COIDisclosure)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("SUBSIDIARY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:SUBSIDIARY]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("ACQUISITION".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:ACQUISITION]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("DISTRIBUTORSHIP".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:DISTRIBUTORSHIP]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("JOINT_VENTURE".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:JOINT_VENTURE]->(e2:Entity)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("PROPOSAL_OWNED_BY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Proposal)-[r1:PROPOSAL_OWNED_BY]->(e2:Unit)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("PROPOSAL_PRIME_SPONSORED_BY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Proposal)-[r1:PROPOSAL_PRIME_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("PROPOSAL_SPONSORED_BY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Proposal)-[r1:PROPOSAL_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("AWARD_OWNED_BY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Award)-[r1:AWARD_OWNED_BY]->(e2:Unit)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("AWARD_PRIME_SPONSORED_BY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Award)-[r1:AWARD_PRIME_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("AWARD_SPONSORED_BY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Award)-[r1:AWARD_SPONSORED_BY]->(e2:Sponsor)  where <WHERE_CLAUSE> RETURN p");
				
			}else if("AWARD_SPONSORED_BY".equalsIgnoreCase(relationship)) {
				
				cypher =  cypher.concat("MATCH p=(e1:Entity)-[r1:ENTITY_AWARD_RELATIONSHIP]->(e2:Award)  where <WHERE_CLAUSE> RETURN p");
				
			}
		
		}
			
		if("Entity".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.entity_number = $param1 OR e2.entity_number = $param1");
			
		}else if("Person".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.person_id = $param1");
			
		}else if("COIDisclosure".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.disclosure_number = $param1");
			
		}else if("Proposal".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.proposal_id = $param1");
			
		}else if("Award".equalsIgnoreCase(request.getNode())) {
			cypher = cypher.replace("<WHERE_CLAUSE>", " e1.award_number = $param1");
			
		}		
	
		
		return cypher;
	}	
	
}
