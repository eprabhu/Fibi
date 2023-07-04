package com.polus.appcoigraph.model;

import java.util.List;

import com.polus.appcoigraph.entity.COIEntity;
import com.polus.appcoigraph.entity.Country;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CountryEntityDTO {

	private Country country;
    private List<COIEntity> actors;
}
