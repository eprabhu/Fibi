var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
import { Injectable } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Observable } from 'rxjs';
import { Constants } from '../../constants/constants.service';
import { HttpClient } from "@angular/common/http";
var AwardReportsAndTermsService = (function () {
    function AwardReportsAndTermsService(http, constant, route) {
        this.http = http;
        this.constant = constant;
        this.route = route;
    }
    AwardReportsAndTermsService.prototype.getAwardReportsAndTerms = function () {
        this.awardId = this.route.snapshot.queryParamMap.get('awardId');
        var params = { 'awardId': this.awardId };
        return this.http.post(this.constant.awardTermsAndReportsUrl, params)
            .catch(function (error) {
            console.error(error.message || error);
            return Observable.throw(error.message || error);
        });
    };
    AwardReportsAndTermsService = __decorate([
        Injectable(),
        __metadata("design:paramtypes", [HttpClient, Constants, ActivatedRoute])
    ], AwardReportsAndTermsService);
    return AwardReportsAndTermsService;
}());
export { AwardReportsAndTermsService };
//# sourceMappingURL=award-reports-and-terms.service.js.map