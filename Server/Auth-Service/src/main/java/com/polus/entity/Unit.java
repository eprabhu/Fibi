package com.polus.entity;;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "UNIT")
public class Unit implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "PARENT_UNIT_NUMBER")
	private String parentUnitNumber;

	@Column(name = "ORGANIZATION_ID")
	private String organizationId;

	@Column(name = "UNIT_NAME")
	private String unitName;

	@Column(name = "ACRONYM")
	private String acronym;


}
