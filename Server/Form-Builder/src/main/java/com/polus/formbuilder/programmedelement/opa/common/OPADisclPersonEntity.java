package com.polus.formbuilder.programmedelement.opa.common;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.Data;

@Entity
@Data
@Table(name = "OPA_DISCL_PERSON_ENTITY")
public class OPADisclPersonEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "OPA_DISCL_PERSON_ENTITY_ID")
    private Integer opaDisclPersonEntityId;

    @Column(name = "OPA_DISCLOSURE_ID", nullable = false)
    private Integer opaDisclosureId;

    @Column(name = "PERSON_ENTITY_ID")
    private Integer personEntityId;
    
    @Column(name = "PERSON_ENTITY_NUMBER")
    private Integer personEntityNumber;

    @Column(name = "ENTITY_ID")
    private Integer entityId;
    
    @Column(name = "ENTITY_NUMBER")
    private Integer entityNumber;
  
    @Column(name = "UPDATE_TIMESTAMP", nullable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private Date updateTimestamp;

    @Column(name = "UPDATE_USER", length = 60, nullable = false)
    private String updateUser;
}
