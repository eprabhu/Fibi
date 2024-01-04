package com.polus.formbuilder.programmedelement.opa.instituteresourceuse;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "OPA_INSTITUTE_RESOURCE_USE")
public class OPAInstituteResourceUseEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "OPA_INST_RES_ID")
	private Integer opaInstResId;

	@Column(name = "OPA_DISCLOSURE_ID")
	private Integer opaDisclosureId;

	@Column(name = "OPA_DISCL_PERSON_ENTITY_ID")
    private Integer opaDisclPersonEntityId;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "DESCRIPTION_1")
	private String description1;

	@Column(name = "DESCRIPTION_2")
	private String description2;

	@Column(name = "UPDATE_TIMESTAMP")
	@Temporal(TemporalType.TIMESTAMP)
	private Date updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

}
