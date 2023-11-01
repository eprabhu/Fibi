package com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest;

import java.math.BigDecimal;
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
@Table(name = "OPA_OUTSIDE_FIN_INTERESTS")
public class OPAOutsideFinancialInterestEntity {

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "OPA_OUTSIDE_FIN_INTEREST_ID")
    private Integer opaOutsideFinancialInterestId;

    @Column(name = "OPA_DISCLOSURE_ID", nullable = false)
    private Integer opaDisclosureId;

    @Column(name = "OPA_DISCL_PERSON_ENTITY_ID")
    private Integer opaDisclPersonEntityId;
	
    @Column(name = "PERSON_REL_WITH_ENTITY", length = 4000)
    private String personsRelationWithEntity;
    
    @Column(name = "ENTITY_REL_WITH_INSTITUTE", length = 4000)
    private String entityRelationWithInstitute;
    
    @Column(name = "DESCRIPTION_1", length = 4000)
    private String description1;
    
    @Column(name = "DESCRIPTION_2", length = 4000)
    private String description2;

    @Column(name = "UPDATE_TIMESTAMP", nullable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private Date updateTimestamp;

    @Column(name = "UPDATE_USER", length = 60, nullable = false)
    private String updateUser;

}
