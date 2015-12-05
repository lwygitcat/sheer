public class Maxpointsonaline {
    public int maxPoints(Point[] points) {
        // Write your code here
        if (points == null || points.length == 0){
            return 0;
        }
        if (points.length < 2) 
            return points.length;
        
        int res = -1;
        for (int i = 0; i < points.length; i++){
            int samex = 1;
            int samep = 0;
            HashMap<Double, Integer> count = new HashMap<Double, Integer>();
            for (int j = i + 1; j < points.length; j++){
               if (points[j].x == points[i].x && points[j].y == points[i].y){
                   samep++;
                }
                if (points[j].x == points[i].x){
                    samex ++;
                    continue;
                }
                double slope = (double)(points[j].y - points[i].y) / (double) (points[j].x - points[i].x);
                if (count.containsKey(slope)){
                    count.put(slope, count.get(slope) + 1);
                } else {
                    count.put(slope, 2);
                }
                res = Math.max(res, count.get(slope) + samep); 
            }
            res = Math.max(res, samex);
        }
        return res;
    }
}





/*
public class Solution {
    public int maxPoints(Point[] points) {
        if (points == null) return 0;
        if(points.length <= 0) return 0;
        if(points.length <= 2) return points.length;
        int result = 0;
        for(int i = 0; i < points.length; i++){
            HashMap<Double, Integer> hm = new HashMap<Double, Integer>();
            int samex = 1;
            int samep = 0;
            for(int j = 0; j < points.length; j++){
                if(j != i){
                    if((points[j].x == points[i].x) && (points[j].y == points[i].y)){
                        samep++;
                    }
                    if(points[j].x == points[i].x){
                        samex++;
                        continue;
                    }
                    double k = (double)(points[j].y - points[i].y) / (double)(points[j].x - points[i].x);
                    if(hm.containsKey(k)){
                        hm.put(k,hm.get(k) + 1);
                    }else{
                        hm.put(k, 2);
                    }
                    result = Math.max(result, hm.get(k) + samep);
                }
            }
            result = Math.max(result, samex);
        }
        return result;
    }
}
*/
