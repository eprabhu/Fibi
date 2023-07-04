package com.polus.appcoigraph.model;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.TargetNode;

import com.polus.appcoigraph.entity.Country;
import com.polus.appcoigraph.entity.COIEntity;
import com.polus.appcoigraph.entity.LinkProperty;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Link {
	@Id
    private Long id;    
    private COIEntity source;
    private Country target;
    private String type;
    private LinkProperty properties;
}
