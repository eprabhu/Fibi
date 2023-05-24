package com.polus.config;

import jakarta.persistence.AttributeConverter;

public class JpaCharBooleanConversion implements AttributeConverter<Boolean, String> {

	public String convertToDatabaseColumn(Boolean b) {
		if (b == null) {
			return null;
		}
		if (b.booleanValue()) {
			return "Y";
		}
		return "N";
	}

	@Override
	public Boolean convertToEntityAttribute(String dbData) {
		// TODO Auto-generated method stub
		return null;
	}


}

