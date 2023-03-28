import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()

export class SfiService {

isShowSfiNavBar = false;

constructor( private _http: HttpClient, private _commonService: CommonService ) { }

}
