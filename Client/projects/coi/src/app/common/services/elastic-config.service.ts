import { Injectable } from '@angular/core';
import {
    ELASTIC_FIBI_PERSON_OUTPUT_FORMAT, ELASTIC_ENTITY_FORMAT
} from '../../app-constants';
import { environment } from '../../../environments/environment';

class ElasticOption {
    url: string;
    formatString: string;
    fields: any;
    index: string;
    type: string;
    contextField: string;
    icons: any;
    formatFields: any;
    size: number;
    extraConditions?: object;
    constructor(url) {
        this.url = url;
    }
}
@Injectable()
export class ElasticConfigService {
    url = '';
    deployMap = environment.deployUrl;
    indexValue = '';

    constructor() { }

    getElasticForPerson() {
        const elasticSearchOption = new ElasticOption(this.url);
        elasticSearchOption.contextField = 'full_name';
        elasticSearchOption.index = 'coiperson' + this.indexValue;
        elasticSearchOption.type = 'person';
        elasticSearchOption.formatString = ELASTIC_FIBI_PERSON_OUTPUT_FORMAT;
        elasticSearchOption.fields = {
            prncpl_id: {}, full_name: {}, prncpl_nm: {}, email_addr: {},
            unit_number: {}, unit_name: {}, addr_line_1: {}, phone_nbr: {},
            is_faculty: {}, is_research_staff: {}
        };
        elasticSearchOption.size = 50;
        return elasticSearchOption;
    }

    getElasticForActiveEntity() {
        const elasticSearchOption = new ElasticOption(this.url);
        elasticSearchOption.contextField = 'entity_name';
        elasticSearchOption.index = 'entity' + this.indexValue;
        elasticSearchOption.type = 'entity';
        elasticSearchOption.formatString = ELASTIC_ENTITY_FORMAT;
        elasticSearchOption.fields = { entity_id: {}, entity_name: {}, country_name: {}, entity_type: {}};
        const elasticSearchOptionFilter = JSON.parse(JSON.stringify(elasticSearchOption));
        elasticSearchOptionFilter.extraConditions = {};
        elasticSearchOptionFilter.extraConditions = {must_not: {match_phrase: {version_status: 'ARCHIVE'}}};
        return JSON.parse(JSON.stringify(elasticSearchOptionFilter));
    }

    getElasticForEntity() {
        const elasticSearchOption = new ElasticOption(this.url);
        elasticSearchOption.contextField = 'entity_name';
        elasticSearchOption.index = 'entity' + this.indexValue;
        elasticSearchOption.type = 'entity';
        elasticSearchOption.formatString = ELASTIC_ENTITY_FORMAT;
        elasticSearchOption.fields = { entity_id: {}, entity_name: {}, country_name: {}, entity_type: {}};
        return elasticSearchOption;
    }

}
