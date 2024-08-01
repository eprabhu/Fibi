package com.polus.integration.proposal.pojo;

import java.io.Serializable;
import java.util.Objects;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class COIIntPropPersonCompositeKey implements Serializable {

	private static final long serialVersionUID = 1L;

	private Integer proposalNumber;

	private String keyPersonRole;

	private Integer keyPersonRoleCode;

	private String keyPersonId;

	@Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        COIIntPropPersonCompositeKey that = (COIIntPropPersonCompositeKey) o;
        return Objects.equals(proposalNumber, that.proposalNumber) &&
                Objects.equals(keyPersonRole, that.keyPersonRole) &&
                Objects.equals(keyPersonRoleCode, that.keyPersonRoleCode) &&
                Objects.equals(keyPersonId, that.keyPersonId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(proposalNumber, keyPersonRole, keyPersonRoleCode, keyPersonId);
    }

}
