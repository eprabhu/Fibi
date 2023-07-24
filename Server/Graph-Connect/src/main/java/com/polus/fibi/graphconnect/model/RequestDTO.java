package com.polus.fibi.graphconnect.model;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
public class RequestDTO {

	private String node;
	
	private String value;
	
	ArrayList<String> relationship;
	
	
}
