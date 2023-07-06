package com.polus.appcoigraph.entity;

import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.RelationshipProperties;
import org.springframework.data.neo4j.core.schema.TargetNode;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@RelationshipProperties
public class LinkProperty {
    
	@Id
    @GeneratedValue
    private Long id;
    private Long elementId;    
    private Long startNodeElementId;    
    private Long endNodeElementId;
    

    @TargetNode
    private Country targetNode;
}
