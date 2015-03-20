## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix(): 此函数用于创建可缓存逆矩阵的特殊“矩阵”对象。
## cacheSolve():此函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。如果已经计算逆矩阵（且尚未更改矩阵）
## 那么cachesolve将检索缓存中的逆矩阵。


## Write a short comment describing this function
## 首先谢谢您的评估。我基本仿照了参考函数，唯一的区别是使用
## solve()函数替代mean()来求矩阵的逆矩阵。
## thanks for your evaluation. I almost "copied" the sample function
## the only difference is I used solve() function to replace the mean()
## function to realize the inverse of a matrix.
## 做过测试成功
## Do test sucessfully.
##

## 函数makeCacheMatrix 通过list 构造出一个特殊的向量，这个向量实质由4个函数
## 代码的组成的列表 ，分别是set（用来设置矩阵），get（用来取得）setinverse(用来设置逆矩阵)
## getinverse(用来取得逆矩阵)
## function makeCacheMatrix used the list function to structure a specialvector
## which in real contains 4 functions code in a list.
## they are : set matrix, get matrix, set inverse matrix and get inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  iv<-NULL
  set<-function(y){
    x<<-y
    iv<<-NULL
  }
  
  get<-function() x
  setinverse<-function(inverse) iv<<-inverse
  getinverse<-function() iv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
  }


## Write a short comment describing this function
## 这个函数先读取ｘ的逆矩阵函数，保存于iv向量，如果iv向量不为空
## 则代表这个逆矩阵已有缓存，读取返回iv的同时提示“获取缓存数据”
##　否则就计算这个矩阵的逆举证，并且把它缓存在iv中。最后输出iv。


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv<-x$getinverse()
  if(!is.null(iv)){
    message("getting cached data")
    return(iv)
  }
  data<- x$get()
  iv <- solve(data)
  x$setinverse(iv)
  iv
}

