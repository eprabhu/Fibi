import { Pipe, PipeTransform } from '@angular/core';
@Pipe({ name: 'orderBy' })
export class OrderrByPipe implements PipeTransform {

    transform(records: Array<any>, args?: any): any {
        return records.sort(function (a, b) {
            if (args.property === 'title') {
                if (a.orcidWork[args.property].toLowerCase() < b.orcidWork[args.property].toLowerCase()) {
                    return -1 * args.direction;
                } else if (a.orcidWork[args.property].toLowerCase() > b.orcidWork[args.property].toLowerCase()) {
                    return 1 * args.direction;
                } else {
                    return 0;
                }
            } else if (args.property === 'sortOrder') {
                if (a.orcidWork.orcidWorkType[args.property] < b.orcidWork.orcidWorkType[args.property]) {
                    return -1 * args.direction;
                } else if (a.orcidWork.orcidWorkType[args.property] > b.orcidWork.orcidWorkType[args.property]) {
                    return 1 * args.direction;
                } else {
                    return 0;
                }
            } else {
                if (!a.orcidWork[args.property]) { a.orcidWork[args.property] =  0; }
                if (!b.orcidWork[args.property]) { b.orcidWork[args.property] =  0; }
                if (parseInt(a.orcidWork[args.property], 10) < parseInt(b.orcidWork[args.property], 10)) {
                    return -1 * args.direction;
                } else if (parseInt(a.orcidWork[args.property], 10) > parseInt(b.orcidWork[args.property], 10)) {
                    return 1 * args.direction;
                } else {
                    return 0;
                }
            }
        });
    }
}
