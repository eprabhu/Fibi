import { Injectable } from '@angular/core';
import { ELASTIC_FIBI_PERSON_OUTPUT_FORMAT } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';

class ElasticOption {
  url: string;
  formatString: string;
  fields: any;
  index: string;
  type: string;
  contextField: string;
  icons: any;
  formatFields: any;
  extraConditions?: object;
  constructor(url) {
      this.url = url;
  }
}

@Injectable()
export class AppElasticService {

  url = '';

  constructor(private _commonService: CommonService) { }

  search(url , param) {
    return new Promise((resolve, reject) => {
        const http = new XMLHttpRequest();
        http.onreadystatechange = function() {
            if (this.readyState === 4 && this.status === 200) {
                resolve(JSON.parse(this.responseText));
            } else if (this.readyState === 4 && this.status !== 200) {
                reject('error');
            }
        };
        http.open('POST', url, true);
        http.setRequestHeader('Content-Type', 'application/json');
        if ( this._commonService.isElasticAuthentiaction ) {
          http.setRequestHeader('Authorization', this._commonService.elasticAuthScheme + ' '
          + btoa(this._commonService.elasticUserName + this._commonService.elasticDelimiter + this._commonService.elasticPassword));
        }
        http.send(JSON.stringify(param));
    });
  }

  getElasticForPerson() {
    const elasticSearchOption = new ElasticOption(this.url);
    elasticSearchOption.contextField = 'full_name';
    elasticSearchOption.index = 'fibiperson';
    elasticSearchOption.type = 'person';
    elasticSearchOption.formatString = ELASTIC_FIBI_PERSON_OUTPUT_FORMAT;
    elasticSearchOption.fields = {
        prncpl_id: {}, full_name: {}, prncpl_nm: {}, email_addr: {},
        unit_number: {}, unit_name: {}, addr_line_1: {}, phone_nbr: {},
        is_faculty: {}, is_research_staff: {}
    };
    return elasticSearchOption;
}

}
