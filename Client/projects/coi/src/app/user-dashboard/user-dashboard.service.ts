import { Injectable } from '@angular/core';
import {HttpClient} from "@angular/common/http";
import {CommonService} from "../common/services/common.service";
import { ActiveDisclosure } from './user-disclosure/user-disclosure-interface';
import { Subject } from 'rxjs';

@Injectable()
export class UserDashboardService {

  activeDisclosures: ActiveDisclosure[] = [];
  activeOPAs = [];
  isModalOpen = false;
  $openModal = new Subject();

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getActiveDisclosure() {
    return this._http.get(this._commonService.baseUrl + '/getActiveDisclosures');
  }

  reviseDisclosure(reviseObject) {
    return this._http.post(this._commonService.baseUrl + '/reviseDisclosure', reviseObject);
  }

  getAllRemaindersList() {
    return this._http.get(this._commonService.baseUrl + '/getAllRemaindersList');
  }

}
