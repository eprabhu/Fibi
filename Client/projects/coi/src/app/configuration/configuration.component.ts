import { Component } from '@angular/core';
import {environment} from "../../../../admin-dashboard/src/environments/environment";
import {CommonService} from "../common/services/common.service";

@Component({
  selector: 'app-configuration',
  templateUrl: './configuration.component.html',
  styleUrls: ['./configuration.component.scss']
})
export class ConfigurationComponent {

  deployMap = environment.deployUrl;

  constructor(private _commonService: CommonService) {
  }

  openInNewTab(path: string) {
    const url = this._commonService.fibiApplicationUrl + '#' + path;
    window.open(url);
  }

}
