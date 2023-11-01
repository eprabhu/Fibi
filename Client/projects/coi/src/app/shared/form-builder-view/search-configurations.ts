export class Endpoint {
    contextField: string;
    formatString: string;
    path: string;
    defaultValue: string;
    params: any;
    filterFields: string;
  }

export function getEndPointForEntity(baseUrl: string): Endpoint {
    const endPointOptions = new Endpoint();
    endPointOptions.contextField = 'entityName';
    endPointOptions.formatString = 'entityName | countryName';
    endPointOptions.path = baseUrl + '/coi/getEntityWithRelationShipInfo';
    endPointOptions.defaultValue = '';
    return endPointOptions;
}

