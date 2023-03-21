//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 05:00:45 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.staffing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Responds with the Event ID of the Move Workers (By Organization) event.
 * 
 * <p>Java class for Move_Workers_By_Organization_ResponseType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Move_Workers_By_Organization_ResponseType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Move_Workers_By_Organization_Reference" type="{urn:com.workday/bsvc}Unique_IdentifierObjectType" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute ref="{urn:com.workday/bsvc}version"/&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Move_Workers_By_Organization_ResponseType", propOrder = {
    "moveWorkersByOrganizationReference"
})
public class MoveWorkersByOrganizationResponseType {

    @XmlElement(name = "Move_Workers_By_Organization_Reference")
    protected UniqueIdentifierObjectType moveWorkersByOrganizationReference;
    @XmlAttribute(name = "version", namespace = "urn:com.workday/bsvc")
    protected String version;

    /**
     * Gets the value of the moveWorkersByOrganizationReference property.
     * 
     * @return
     *     possible object is
     *     {@link UniqueIdentifierObjectType }
     *     
     */
    public UniqueIdentifierObjectType getMoveWorkersByOrganizationReference() {
        return moveWorkersByOrganizationReference;
    }

    /**
     * Sets the value of the moveWorkersByOrganizationReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link UniqueIdentifierObjectType }
     *     
     */
    public void setMoveWorkersByOrganizationReference(UniqueIdentifierObjectType value) {
        this.moveWorkersByOrganizationReference = value;
    }

    /**
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVersion(String value) {
        this.version = value;
    }

}
