package com.polus.appcoigraph.model;

import java.util.ArrayList;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ResponseDTO {

	private ArrayList<Map<String, Object>> nodes;
    private ArrayList<Map<String, Object>> links;
    
}
