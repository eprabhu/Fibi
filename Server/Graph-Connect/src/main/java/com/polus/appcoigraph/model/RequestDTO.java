package com.polus.appcoigraph.model;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RequestDTO {

	private String node;
	
	private String value;
	
	ArrayList<String> relationship;
	
}
